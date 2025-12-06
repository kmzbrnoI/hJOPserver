unit JCBarriers;

{
  Co je to BARIERA JIZDNI CESTY?
  > Bariera jizdni cesty je prekazka branici jejimu postaveni, ktere se lze
  > zbavit napr. jen pouhym potvrzenim (napr. jizdni cesta pres blok se stitkem),
  > potvrzenim pres rizikovou funkci, nebo se ji nelze zbavit vubec a jizdni
  > cestu jendnoduse nelze postavit.

  Technologie jizdnch cest rozeznava nekolik typu:
  1) KRITICKE BARIERY
    jsou takove bariery, ktere vypravci nemuze odstranit (v ceste napriklad
    chybi technologicky blok).
  2) STANDARDNI BARIERY
    jsou takove bariery, ktere se odstrani "samy" - napriklad usek pod
    zaverem, obsazney usek.
    > Standardni bariera typicky neni kriticka, ani neni varovna, tudiz
    > se pozna tak, ze nesplnuje podminky kriticke ani varovne bariery.
  3) VAROVNE BARIERY
    jsou takove bariery, ktere primo nebrani jizdni ceste ve staveni, ale je
    potreba si je uvedmit a potvrdit je (napr. na useku je stitek, ci vyluka).
    Tyto bariery je vzdy nutne potvrdit upozornenim v levem dolnim rohu panelu,
    nektere z nich mohou vyzadovat i rizikovou funkci.
}

interface

uses Block, Generics.Collections, Area, JsonDataObjects, UPO, ConfSeq, SysUtils;

type
  EJCBarrier = class(Exception)
  end;

  TJCBarrier = class
  public
    function Equal(other: TJCBarrier): Boolean; virtual;
    function ToUPO(): TUPOItem; virtual;
    function ToConfSeq(): TConfSeqItem; virtual;
    function IsCritical(): Boolean; virtual;
    function IsWarning(): Boolean; virtual;
    function IsRisky(): Boolean; virtual;
    function CanContinueByConfirm(): Boolean;
    function RiskyNote(): string; virtual;
    function MustPassForDN(): Boolean; virtual;
    procedure ToJson(Result: TJsonObject); virtual;
  end;

  TJCBlockBarrier = class(TJCBarrier)
  private
    mBlock: TBlk;
    function BlockerUPO(line1: string): TUPOItem;
  public
    constructor Create(block: TBlk);
    property block: TBlk read mBlock;
    procedure ToJson(Result: TJsonObject); override;
    function Equal(other: TJCBarrier): Boolean; override;
  end;

  // -----------------------------------------------------------------------
  // General barriers

  TJCBarProcessing = class(TJCBarrier)
  public
    function IsCritical(): Boolean; override;
    function ToUPO(): TUPOItem; override;
    function MustPassForDN(): Boolean; override;
  end;

  TJCBarGeneralNote = class(TJCBarrier)
  private
    mText: string;

  public
    constructor Create(text: string);
    function ToUPO(): TUPOItem; override;
    function IsWarning(): Boolean; override;
    property text: string read mText;
  end;

  TJCBarBlockNotExists = class(TJCBarrier)
  private
    mId: Integer;
  public
    constructor Create(id: Integer);
    function ToUPO(): TUPOItem; override;
    function IsCritical(): Boolean; override;
    function Equal(other: TJCBarrier): Boolean; override;
  end;

  TJCBarBlockWrongType = class(TJCBlockBarrier)
  public
    function ToUPO(): TUPOItem; override;
    function IsCritical(): Boolean; override;
  end;

  TJCBarBlockDisabled = class(TJCBlockBarrier)
  private
    const MSG: string = 'Blok neaktivní';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
    function IsCritical(): Boolean; override;
  end;

  TJCBarPrivol = class(TJCBlockBarrier)
  public
    function ToUPO(): TUPOItem; override;
    function IsWarning(): Boolean; override;
  end;

  TJCBarBlockNote = class(TJCBlockBarrier)
  public
    function ToUPO(): TUPOItem; override;
    function IsWarning(): Boolean; override;
  end;

  TJCBarBlockLockout = class(TJCBlockBarrier)
    function ToUPO(): TUPOItem; override;
    function IsWarning(): Boolean; override;
    function IsRisky(): Boolean; override;
    function RiskyNote(): string; override;
  end;

  TJCBarBlockPSt = class(TJCBlockBarrier)
  private
    const MSG: string = 'Prvek pod pomocným stavědlem';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
  end;

  // -----------------------------------------------------------------------
  // Signal

  TJCBarSignalNoTrack = class(TJCBlockBarrier)
  private
    const MSG: string = 'Není úsek před návěstidlem';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
  end;

  TJCBarSignalActive = class(TJCBlockBarrier)
  private
    const MSG: string = 'Není základní návěst';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
    function MustPassForDN(): Boolean; override;
  end;

  // -----------------------------------------------------------------------
  // Track

  TJCBarTrackOccupied = class(TJCBlockBarrier)
  private
    const MSG: string = 'Úsek obsazen';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
  end;

  TJCBarTrackZaver = class(TJCBlockBarrier)
  private
    const MSG: string = 'Úsek zapevněn';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
    function MustPassForDN(): Boolean; override;
  end;

  TJCBarTrackTrain = class(TJCBlockBarrier)
  private
    const MSG: string = 'Úsek obsahuje vlak';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
  end;

  TJCBarTrackAB = class(TJCBlockBarrier)
  private
    const MSG: string = 'Blokováno automatickou JC';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
  end;

  TJCBarTrackLastOccupied = class(TJCBlockBarrier)
  public
    class function ToUPO(block: TBlk): TUPOItem; overload;
    function ToUPO(): TUPOItem; overload; override;
    function IsWarning(): Boolean; override;
  end;

  TPStBarTrackOccupied = class(TJCBlockBarrier)
  public
    function ToUPO(): TUPOItem; overload; override;
    function IsWarning(): Boolean; override;
  end;

  // -----------------------------------------------------------------------
  // Turnout

  TJCBarTurnoutNoPos = class(TJCBlockBarrier)
  private
    const MSG: string = 'Není koncová poloha'; // TODO tady byl preklep na 'neni spravna poloha', zkontroloval u VC/NC
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
  end;

  TJCBarTurnoutLocked = class(TJCBlockBarrier)
  private
    const MSG: string = 'Zamčena';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
    function MustPassForDN(): Boolean; override;
  end;

  TJCBarTurnoutEmLock = class(TJCBlockBarrier)
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
  end;

  TJCBarTurnoutWrongPos = class(TJCBlockBarrier)
  private
    const MSG: string = 'Nesprávná poloha';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
  end;

  TJCBarTurnoutOccupied = class(TJCBlockBarrier) // used in refugees
  private
    const MSG: string = 'Obsazena';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
  end;

  // -----------------------------------------------------------------------
  // Crossing

  TJCBarCrossingEmergencyOpened = class(TJCBlockBarrier)
  private
    const MSG: string = 'Nouzově otevřen';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
  end;

  TJCBarCrossingError = class(TJCBlockBarrier)
  private
    const MSG: string = 'Poruchový stav';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
  end;

  TJCBarCrossingNotClosed = class(TJCBlockBarrier)
  private
    const MSG: string = 'Neuzavřen';
  public
    function ToConfSeq(): TConfSeqItem; override;
  end;

  // -----------------------------------------------------------------------
  // Railway

  TJCBarRailwayNotReady = class(TJCBlockBarrier)
  private
    const MSG: string = 'Nepovoluje odjezd';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
    function MustPassForDN(): Boolean; override; // TODO really ???
  end;

  TJCBarRailwayZAKVC = class(TJCBlockBarrier)
  private
    const MSG: string = 'Zaveden zákaz odjezdu';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
  end;

  TJCBarRailwayZAKPC = class(TJCBlockBarrier)
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
    function IsWarning(): Boolean; override;
    function IsRisky(): Boolean; override;
    function RiskyNote(): string; override;
  end;

  TJCBarRailwayZaver = class(TJCBlockBarrier)
  private
    const MSG: string = 'Závěr';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
    function MustPassForDN(): Boolean; override;
  end;

  TJCBarRailwayOccupied = class(TJCBlockBarrier)
  public
    function ToUPO(): TUPOItem; override;
    function IsWarning(): Boolean; override;    // TODO MustPassForDN was false origiannly but should be true, right?
  end;

  TJCBarRailwayRequesting = class(TJCBlockBarrier)
  private
    const MSG: string = 'Probíhá žádost';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
  end;

  TJCBarRailwayWrongDir = class(TJCBlockBarrier)
  private
    const MSG: string = 'Nesouhlas';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
  end;

  TJCBarRailwayNoBP = class(TJCBlockBarrier)
  private
    const MSG: string = 'Bloková podmínka nezavedena';
  public
    function ToConfSeq(): TConfSeqItem; override;
  end;

  TJCBarRailwayNoZAK = class(TJCBlockBarrier)
  private
    const MSG: string = 'Nezaveden zákaz odjezdu';
  public
    function ToConfSeq(): TConfSeqItem; override;
  end;

  TJCBarRailwayNoTrainMove = class(TJCBlockBarrier)
  private
    const MSG: string = 'Nedojde k přenosu čísla vlaku';
  public
    function ToConfSeq(): TConfSeqItem; override;
  end;

  TJCBarRailwayMoveToEnd = class(TJCBlockBarrier)
  private
    const MSG: string = 'Vlak bude přenesen až na konec trati';
  public
    function ToConfSeq(): TConfSeqItem; override;
  end;

  // -----------------------------------------------------------------------
  // Lock

  TJCBarLockNotLocked = class(TJCBlockBarrier)
  private
    const MSG: string = 'Neuzamčen';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
  end;

  TJCBarLockEmLock = class(TJCBlockBarrier)
  private
    const MSG: string = 'Není zaveden nouzový závěr';
  public
    function ToConfSeq(): TConfSeqItem; override;
  end;

  // -----------------------------------------------------------------------
  // Disconnector

  TPStBarDisconnectorActive = class(TJCBlockBarrier)
  private
    const MSG: string = 'Rozpojovač aktivní';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
  end;

  // -----------------------------------------------------------------------
  // PSt

  TPStBarControllerNotInBasicPos = class(TJCBlockBarrier)
  private
    const MSG: string = 'Volič není v základní poloze';
  public
    function ToUPO(): TUPOItem; override;
    function ToConfSeq(): TConfSeqItem; override;
  end;

  TPStBarControllerNotInBasicPosWarn = class(TJCBlockBarrier)
  public
    function ToUPO(): TUPOItem; override;
    function IsWarning(): Boolean; override;
  end;

  // -----------------------------------------------------------------------
  // Vehicles & trains

  TJCBarTrainWrongDir = class(TJCBarrier)
  private
    mTrainI: Integer;
  public
    constructor Create(traini: Integer);
    function ToUPO(): TUPOItem; override;
    function IsWarning(): Boolean; override;
    function Equal(other: TJCBarrier): Boolean; override;
  end;

  TJCBarRVManual = class(TJCBarrier)
  private
    mAddr: Integer;
  public
    constructor Create(addr: Integer);
    class function ToUPO(addr: Integer): TUPOItem; overload;
    function ToUPO(): TUPOItem; overload; override;
    function IsWarning(): Boolean; override;
    function Equal(other: TJCBarrier): Boolean; override;
  end;

  TJCBarTrainNotFront = class(TJCBarrier)
  private
    mTrainI: Integer;
  public
    constructor Create(traini: Integer);
    function ToUPO(): TUPOItem; override;
    function IsWarning(): Boolean; override;
    function Equal(other: TJCBarrier): Boolean; override;
  end;

  TJCBarRVNotAllManual = class(TJCBarrier)
  private
    mTrainI: Integer;
  public
    constructor Create(traini: Integer);
    function ToUPO(): TUPOItem; override;
    function IsWarning(): Boolean; override;
  end;

  TJCBarRVFuncActive = class(TJCBarrier)
  private
    mAddr: Integer;
    mFunc: Cardinal;
  public
    constructor Create(addr: Integer; func: Cardinal);
    function ToUPO(): TUPOItem; override;
    function IsWarning(): Boolean; override;
    function Equal(other: TJCBarrier): Boolean; override;
  end;

  // -----------------------------------------------------------------------

  TJCBarriers = class(TObjectList<TJCBarrier>)
    function Add(const Value: TJCBarrier): Integer;
  end;

  function BarriersToConfSeq(barriers: TJCBarriers): TConfSeqItems;
  function IsAnyCriticalButProcessing(barriers: TJCBarriers): Boolean;
  function CanContinueByConfirmButProcessing(barriers: TJCBarriers): Boolean;
  function IsAnyRiskyBarrier(barriers: TJCBarriers): Boolean;

implementation

uses Graphics, Classes, BlockTurnout, BlockTrack, BlockPst, BlockLock,
  BlockCrossing, BlockLinker, BlockDisconnector, TRVDatabase, TrainDb, colorHelper,
  TRailVehicle;

function TJCBarrier.Equal(other: TJCBarrier): Boolean;
begin
  Result := (Self.ClassName = other.ClassName);
end;

function TJCBarriers.Add(const Value: TJCBarrier): Integer;
begin
  for var item: TJCBarrier in Self do
    if (item.Equal(Value)) then
      Exit(1);
  Result := inherited Add(Value);
end;

function TJCBarrier.ToUPO(): TUPOItem;
begin
  raise EJCBarrier.Create('ToUPO not implemented for the barrier');
end;

function TJCBarrier.ToConfSeq(): TConfSeqItem;
begin
  raise EJCBarrier.Create('ToConfSeq not implemented for the barrier');
end;

function TJCBarrier.IsCritical(): Boolean;
begin
  Result := False;
end;

function TJCBarrier.IsWarning(): Boolean;
begin
  Result := False;
end;

function TJCBarrier.IsRisky(): Boolean;
begin
  Result := False;
end;

function TJCBarrier.RiskyNote(): string;
begin
  Result := '';
end;

function TJCBarrier.MustPassForDN(): Boolean;
begin
  Result := (not Self.IsWarning());
end;

procedure TJCBarrier.ToJson(Result: TJsonObject);
begin
  Result['typ'] := Self.ClassName;

  if (Self.IsCritical()) then
    Result['type'] := 'critical'
  else if (Self.IsWarning()) then
    Result['type'] := 'warning'
  else
    Result['type'] := 'standard';

  var upoItem := Self.ToUPO();
  Result['description'] := upoItem[0].str + ' ' + upoItem[1].str + ' ' + upoItem[2].str;
end;

function TJCBarrier.CanContinueByConfirm(): Boolean;
begin
  Result := (not Self.IsCritical()) and (Self.IsWarning());
end;

function TJCBlockBarrier.BlockerUPO(line1: string): TUPOItem;
begin
  Result[0] := GetUPOLine('NEPŘÍPUSTNÉ', taCenter, TJopColor.red, TJopColor.white);
  Result[1] := GetUPOLine(line1);
  Result[2] := GetUPOLine(Self.block.name);
end;

constructor TJCBlockBarrier.Create(block: TBlk);
begin
  inherited Create();
  if (block = nil) then
    raise EJCBarrier.Create('Cannot create barrier with block=nil!');
  Self.mBlock := block;
end;

procedure TJCBlockBarrier.ToJson(Result: TJsonObject);
begin
  inherited;
  if (Self.block <> nil) then
    Result['block'] := Self.block.id;
end;

function TJCBlockBarrier.Equal(other: TJCBarrier): Boolean;
begin
  Result := inherited;
  if ((Result) and (other.InheritsFrom(TJCBlockBarrier))) then
    Result := (Result) and (Self.mBlock = (other as TJCBlockBarrier).mBlock);
end;

function TJCBarProcessing.IsCritical(): Boolean;
begin
  Result := True;
end;

function TJCBarProcessing.ToUPO(): TUPOItem;
begin
  Result[0] := GetUPOLine('Probíhá stavění', taCenter, TJopColor.blue, TJopColor.grayDark);
  Result[1] := GetUPOLine('', taCenter, TJopColor.blue, TJopColor.grayDark);
  Result[2] := GetUPOLine('', taCenter, TJopColor.blue, TJopColor.grayDark);
end;

function TJCBarProcessing.MustPassForDN(): Boolean;
begin
  Result := False;
end;

constructor TJCBarGeneralNote.Create(text: string);
begin
  inherited Create();
  Self.mText := text;
end;

function TJCBarGeneralNote.ToUPO(): TUPOItem;
begin
  Result := NoteUPO('', Self.text);
end;

function TJCBarGeneralNote.IsWarning(): Boolean;
begin
  Result := True;
end;

constructor TJCBarBlockNotExists.Create(id: Integer);
begin
  inherited Create();
  Self.mId := id;
end;

function TJCBarBlockNotExists.ToUPO(): TUPOItem;
begin
  Result[0] := GetUPOLine('NEPŘÍPUSTNÉ', taCenter, TJopColor.red, TJopColor.white);
  Result[1] := GetUPOLine('Blok neexistuje');
  Result[2] := GetUPOLine('ID ' + IntToStr(Self.mId));
end;

function TJCBarBlockNotExists.IsCritical(): Boolean;
begin
  Result := True;
end;

function TJCBarBlockNotExists.Equal(other: TJCBarrier): Boolean;
begin
  Result := inherited;
  if ((Result) and (other.InheritsFrom(TJCBarBlockNotExists))) then
    Result := (Result) and (Self.mId = (other as TJCBarBlockNotExists).mId);
end;

function TJCBarBlockWrongType.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO('Blok není správného typu');
end;

function TJCBarBlockWrongType.IsCritical(): Boolean;
begin
  Result := True;
end;

function TJCBarBlockDisabled.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarBlockDisabled.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarBlockDisabled.IsCritical(): Boolean;
begin
  Result := True;
end;

function TJCBarPrivol.ToUPO(): TUPOItem;
begin
  Result := UPO.PNUPO(Self.block.name);
end;

function TJCBarBlockNote.ToUPO(): TUPOItem;
begin
  var note: string := '-';
  case (Self.block.typ) of
    btTurnout: note := TBlkTurnout(Self.block).note;
    btTrack, btRT: note := TBlkTrack(Self.block).note;
    btCrossing: note := TBlkCrossing(Self.block).note;
    btLinker: note := TBlkLinker(Self.block).note;
    btLock: note := TBlkLock(Self.block).note;
    btDisconnector: note := TBlkDisconnector(Self.block).note;
    btPst: note := TBlkPst(Self.block).note;
  end;
  Result := UPO.NoteUPO(Self.block.name, note);
end;

function TJCBarBlockNote.IsWarning(): Boolean;
begin
  Result := True;
end;

function TJCBarBlockLockout.ToUPO(): TUPOItem;
begin
  var lockout: string := '-';
  case (Self.block.typ) of
    btTurnout: lockout := TBlkTurnout(Self.block).lockout;
    btTrack, btRT: lockout := TBlkTrack(Self.block).lockout;
    btCrossing: lockout := TBlkCrossing(Self.block).lockout;
  end;
  Result := LockoutUPO(Self.block.name, lockout);
end;

function TJCBarBlockLockout.IsWarning(): Boolean;
begin
  Result := True;
end;

function TJCBarBlockLockout.IsRisky(): Boolean;
begin
  Result := True;
end;

function TJCBarBlockLockout.RiskyNote(): string;
begin
  Result := 'Výluka bloku';
end;

function TJCBarPrivol.IsWarning(): Boolean;
begin
  Result := True;
end;

function TJCBarBlockPSt.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarBlockPSt.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarSignalNoTrack.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarSignalNoTrack.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarSignalActive.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarSignalActive.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarSignalActive.MustPassForDN(): Boolean;
begin
  Result := False;
end;

function TJCBarTrackOccupied.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarTrackOccupied.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarTrackZaver.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarTrackZaver.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarTrackZaver.MustPassForDN(): Boolean;
begin
  Result := False;
end;

function TJCBarTrackTrain.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarTrackTrain.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarTrackAB.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarTrackAB.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

class function TJCBarTrackLastOccupied.ToUPO(block: TBlk): TUPOItem;
begin
  Result[0] := GetUPOLine('NEBUDE POVOLUJÍCÍ NÁVĚST', taCenter, TJopColor.black, TJopColor.yellow);
  Result[1] := GetUPOLine('Kolejový úsek obsazen');
  Result[2] := GetUPOLine(block.name);
end;

function TJCBarTrackLastOccupied.ToUPO(): TUPOItem;
begin
  Result := TJCBarTrackLastOccupied.ToUPO(Self.block);
end;

function TJCBarTrackLastOccupied.IsWarning(): Boolean;
begin
  Result := True;
end;

function TPStBarTrackOccupied.ToUPO(): TUPOItem;
begin
  Result[0] := GetUPOLine('POZOR !', taCenter, clBlack, clYellow);
  Result[1] := GetUPOLine('Úsek obsazen');
  Result[2] := GetUPOLine(block.name);
end;

function TPStBarTrackOccupied.IsWarning(): Boolean;
begin
  Result := True;
end;

function TJCBarTurnoutNoPos.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarTurnoutNoPos.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarTurnoutLocked.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarTurnoutLocked.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarTurnoutLocked.MustPassForDN(): Boolean;
begin
  Result := False;
end;

function TJCBarTurnoutEmLock.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO('Nouzový závěr');
end;

function TJCBarTurnoutEmLock.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, 'Není zaveden nouzový závěr');
end;

function TJCBarTurnoutWrongPos.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarTurnoutWrongPos.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarCrossingEmergencyOpened.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarCrossingEmergencyOpened.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarCrossingError.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarCrossingError.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarCrossingNotClosed.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarTurnoutOccupied.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarTurnoutOccupied.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarRailwayNotReady.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarRailwayNotReady.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarRailwayNotReady.MustPassForDN(): Boolean;
begin
  Result := False;
end;

function TJCBarRailwayZAKVC.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarRailwayZAKVC.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarRailwayZAKPC.ToUPO(): TUPOItem;
begin
  Result[0] := GetUPOLine('ZAVEDEN ZÁKAZ ODJEZDU', taCenter, TJopColor.red, TJopColor.white);
  Result[1] := GetUPOLine(Self.block.name);
  Result[2] := GetUPOLine('');
end;

function TJCBarRailwayZAKPC.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, 'Zaveden zákaz odjezdu');
end;

function TJCBarRailwayZAKPC.IsWarning(): Boolean;
begin
  Result := True;
end;

function TJCBarRailwayZAKPC.IsRisky(): Boolean;
begin
  Result := True;
end;

function TJCBarRailwayZAKPC.RiskyNote(): string;
begin
  Result := 'Zákaz odjezdu na trať';
end;

function TJCBarRailwayZaver.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarRailwayZaver.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarRailwayZaver.MustPassForDN(): Boolean;
begin
  Result := False;
end;

function TJCBarRailwayOccupied.ToUPO(): TUPOItem;
begin
  Result[0] := GetUPOLine('NEBUDE POVOLUJÍCÍ NÁVĚST', taCenter, TJopColor.black, TJopColor.yellow);
  Result[1] := GetUPOLine('Trať obsazena');
  Result[2] := GetUPOLine(Self.block.name);
end;

function TJCBarRailwayOccupied.IsWarning(): Boolean;
begin
  Result := True;
end;

function TJCBarRailwayRequesting.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarRailwayRequesting.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarRailwayWrongDir.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarRailwayWrongDir.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarRailwayNoBP.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarRailwayNoZAK.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarRailwayNoTrainMove.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarRailwayMoveToEnd.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarLockNotLocked.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TJCBarLockNotLocked.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TJCBarLockEmLock.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TPStBarDisconnectorActive.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TPStBarDisconnectorActive.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TPStBarControllerNotInBasicPos.ToUPO(): TUPOItem;
begin
  Result := Self.BlockerUPO(Self.MSG);
end;

function TPStBarControllerNotInBasicPos.ToConfSeq(): TConfSeqItem;
begin
  Result := CSItem(Self.block, Self.MSG);
end;

function TPStBarControllerNotInBasicPosWarn.ToUPO(): TUPOItem;
begin
  Result[0] := GetUPOLine('POZOR !', taCenter, TJopColor.black, TJopColor.yellow);
  Result[1] := GetUPOLine('Volič není v základní poloze');
  Result[2] := GetUPOLine(Self.block.name);
end;

function TPStBarControllerNotInBasicPosWarn.IsWarning(): Boolean;
begin
  Result := True;
end;

constructor TJCBarRVManual.Create(addr: Integer);
begin
  inherited Create();
  Self.mAddr := addr;
end;

class function TJCBarRVManual.ToUPO(addr: Integer): TUPOItem;
begin
  if (RVDb[addr] = nil) then
    raise EJCBarrier.Create('Engine does not exist');

  Result[0] := GetUPOLine('Vozidlo v ručním řízení', taCenter, TJopColor.black, TJopColor.yellow);
  Result[1] := GetUPOLine(IntToStr(addr) + ' : ' + RVDb[addr].name);
  var drivers: string := RVDb[addr].DriverFullNames();
  if (drivers <> '') then
    Result[2] := GetUPOLine('Řídí: '+drivers);
end;

function TJCBarRVManual.ToUPO(): TUPOItem;
begin
  Result := TJCBarRVManual.ToUPO(Self.mAddr);
end;

function TJCBarRVManual.IsWarning(): Boolean;
begin
  Result := True;
end;

function TJCBarRVManual.Equal(other: TJCBarrier): Boolean;
begin
  Result := inherited;
  if ((Result) and (other.InheritsFrom(TJCBarRVManual))) then
    Result := (Result) and (Self.mAddr = (other as TJCBarRVManual).mAddr);
end;

constructor TJCBarTrainNotFront.Create(traini: Integer);
begin
  inherited Create();
  Self.mTrainI := traini;
end;

function TJCBarTrainNotFront.ToUPO(): TUPOItem;
begin
  if (trains[Self.mTrainI] = nil) then
    raise EJCBarrier.Create('Train does not exist');

  Result[0] := GetUPOLine('POZOR !', taCenter, TJopColor.black, TJopColor.yellow);
  Result[1] := GetUPOLine('Čelo vlaku je na jiném úseku');
  Result[2] := GetUPOLine('Vlak ' + trains[Self.mTrainI].name);
end;

function TJCBarTrainNotFront.IsWarning(): Boolean;
begin
  Result := True;
end;

function TJCBarTrainNotFront.Equal(other: TJCBarrier): Boolean;
begin
  Result := inherited;
  if ((Result) and (other.InheritsFrom(TJCBarTrainNotFront))) then
    Result := (Result) and (Self.mTrainI = (other as TJCBarTrainNotFront).mTrainI);
end;

constructor TJCBarRVNotAllManual.Create(traini: Integer);
begin
  inherited Create();
  Self.mTrainI := traini;
end;

function TJCBarRVNotAllManual.ToUPO(): TUPOItem;
begin
  if (trains[Self.mTrainI] = nil) then
    raise EJCBarrier.Create('Train does not exist');

  Result[0] := GetUPOLine('POZOR !', taCenter, TJopColor.black, TJopColor.yellow);
  Result[1] := GetUPOLine('Ne všechna vozidla v ručním řízení');
  Result[2] := GetUPOLine('Vlak ' + trains[Self.mTrainI].name);
end;

function TJCBarRVNotAllManual.IsWarning(): Boolean;
begin
  Result := True;
end;

constructor TJCBarTrainWrongDir.Create(traini: Integer);
begin
  inherited Create();
  Self.mTrainI := traini;
end;

function TJCBarTrainWrongDir.ToUPO(): TUPOItem;
begin
  if (trains[Self.mTrainI] = nil) then
    raise EJCBarrier.Create('Train does not exist');

  Result[0] := GetUPOLine('POZOR !', taCenter, TJopColor.black, TJopColor.yellow);
  Result[1] := GetUPOLine('Jízda proti směru vlaku');
  Result[2] := GetUPOLine('Vlak ' + trains[Self.mTrainI].name);
end;

function TJCBarTrainWrongDir.IsWarning(): Boolean;
begin
  Result := True;
end;

function TJCBarTrainWrongDir.Equal(other: TJCBarrier): Boolean;
begin
  Result := inherited;
  if ((Result) and (other.InheritsFrom(TJCBarTrainWrongDir))) then
    Result := (Result) and (Self.mTrainI = (other as TJCBarTrainWrongDir).mTrainI);
end;

constructor TJCBarRVFuncActive.Create(addr: Integer; func: Cardinal);
begin
  Self.mAddr := addr;
  Self.mFunc := func;
end;

function TJCBarRVFuncActive.ToUPO(): TUPOItem;
begin
  if (RVDb[Self.mAddr] = nil) then
    raise EJCBarrier.Create('Engine does not exist');
  if (Self.mFunc > _RV_FUNC_MAX) then
    raise EJCBarrier.Create('mFunc out of range');

  var description: string := RVDb[Self.mAddr].data.funcDescription[Self.mFunc];
  var line1: string := 'Aktivní funkce F'+IntToStr(Self.mFunc);
  if (description <> '') then
    line1 := line1 + ' : ' + description;

  Result[0] := GetUPOLine('POZOR !', taCenter, TJopColor.black, TJopColor.yellow);
  Result[1] := GetUPOLine(line1);
  Result[2] := GetUPOLine(IntToStr(Self.mAddr) + ' : ' + RVDb[Self.mAddr].name);
end;

function TJCBarRVFuncActive.IsWarning(): Boolean;
begin
  Result := True;
end;

function TJCBarRVFuncActive.Equal(other: TJCBarrier): Boolean;
begin
  Result := inherited;
  if ((Result) and (other.InheritsFrom(TJCBarRVFuncActive))) then
    Result := (Result) and (Self.mAddr = (other as TJCBarRVFuncActive).mAddr) and (Self.mFunc = (other as TJCBarRVFuncActive).mFunc);
end;

/// /////////////////////////////////////////////////////////////////////////////

function BarriersToConfSeq(barriers: TJCBarriers): TConfSeqItems;
begin
  Result := TList<TConfSeqItem>.Create();

  try
    for var barrier: TJCBarrier in barriers do
      Result.Add(barrier.ToConfSeq());
  except
    Result.Free();
    raise;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function IsAnyRiskyBarrier(barriers: TJCBarriers): Boolean;
begin
  for var barrier: TJCBarrier in barriers do
    if (barrier.IsRisky()) then
      Exit(True);
  Result := False;
end;

function IsAnyCriticalButProcessing(barriers: TJCBarriers): Boolean;
begin
  for var barrier in barriers do
    if ((barrier.ClassType <> TJCBarProcessing) and (barrier.IsCritical())) then
      Exit(True);
  Result := False;
end;

function CanContinueByConfirmButProcessing(barriers: TJCBarriers): Boolean;
begin
  for var barrier in barriers do
    if ((barrier.ClassType <> TJCBarProcessing) and (not barrier.CanContinueByConfirm())) then
      Exit(False);
  Result := True;
end;

end.
