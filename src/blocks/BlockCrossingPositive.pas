unit BlockCrossingPositive;

{
 Setting a positive light of a crossing

 There is a list of 'rules'.
 Each 'rule' consists of a 'conditions' and 'blocks'.

 'Conditions' is a list of conditions. Each condition is in format turnout:position (e.g. Sk 1:+)
 Conditions are met if each turnout in the conditions has the defined position OR no position OR error position.
 If the condition is met, state of 'blocks' is evaluated.

 'Blocks' is a simple list of blocks, which can contain types: track, signal.
 If all the blocks are in positive-on state, the rule is in positive-on state. Else the rule is in positive-off state.
 The track is in positive-on state iff it is not occupied.
 The signal is in positive-on state iff it is transmits the 'stop' aspect and no other aspect.

 If any rule with met condition is in positive-off state, the positive is off. Else the positive is on.

 Notes:
   The 'rules' list may be empty. Empty list is always 'met'.
   Empty 'blocks' list is considered as an error, in this state rule is considered as positive-off (safe state).

 Consequences:
   No rules -> positive on
   No rules with met conditions -> positive on
}

interface

uses BlockTurnout, Generics.Collections, BlockProxy, Classes, SysUtils, Block,
  StrUtils;

type
  TPositiveCondition = class
  private
    // turnout proxy so we don't have to get turnout from id in each call of 'Met'
    proxy: TBlockProxy;

     function GetTurnoutBlk(): TBlkTurnout;

  public
    turnout: Integer;
    position: TTurnoutPosition;

     constructor Create(); overload;
     constructor Create(str: string); overload;
     destructor Destroy(); override;

     procedure Parse(str: string);
     function IdStr(): string;
     function NameStr(): string;

     function Met(): Boolean;
     property turnoutBlk: TBlkTurnout read GetTurnoutBlk;
  end;

  TPositiveRule = class
  private
    blockProxies: TObjectList<TBlockProxy>;

     procedure ParseConditions(conds: string);
     procedure ParseBlocks(blocks: string);
     function BlockPositiveOn(blk: TBlk): Boolean;

  public
    // if conditions are met, the rule is used
    conditions: TObjectList<TPositiveCondition>;
    blocks: TList<Integer>;

     constructor Create(); overload;
     constructor Create(line: string); overload;
     destructor Destroy(); override;

     procedure Parse(line: string);
     function IdStr(): string;
     function NameStr(): string;

     function PositiveOn(): Boolean;
     function ConditionsMet(): Boolean;
     function BlocksPositiveOn(): Boolean;
  end;

  TPositiveRules = TObjectList<TPositiveRule>;

  function PositiveOn(rules: TPositiveRules): Boolean;

implementation

uses BlockDb, ownStrUtils, BlockSignal, BlockTrack, ownConvert;

////////////////////////////////////////////////////////////////////////////////

constructor TPositiveCondition.Create();
begin
  inherited;
  Self.proxy := TBlockProxy.Create();
end;

constructor TPositiveCondition.Create(str: string);
begin
  Self.Create();
  Self.Parse(str);
end;

destructor TPositiveCondition.Destroy();
begin
  Self.proxy.Free();
  inherited;
end;

function TPositiveCondition.GetTurnoutBlk(): TBlkTurnout;
begin
  var blk: TBlk := Self.proxy.Block(Self.turnout);
  if (blk.typ = TBlkType.btTurnout) then
    Result := blk as TBlkTurnout
  else
    Result := nil;
end;

function TPositiveCondition.Met(): Boolean;
begin
  Result := (Self.turnoutBlk.position = Self.position) or (Self.turnoutBlk.position = TTurnoutPosition.both) or
    (Self.turnoutBlk.position = TTurnoutPosition.disabled) or (Self.turnoutBlk.position = TTurnoutPosition.none);
end;

procedure TPositiveCondition.Parse(str: string);
begin
  var strs: TStrings := TStringList.Create();
  try
    ExtractStringsEx([':'], [' '], str, strs);
    if (strs.Count <> 2) then
      raise EConvertError.Create(str + 'is not in format "turnoutid:[+-]" (TPositiveCondition.Parse)');
    Self.turnout := StrToInt(strs[0]);
    Self.position := TBlkTurnout.StrToPosition(strs[1]);
    if ((Self.position <> TTurnoutPosition.plus) and (Self.position <> TTurnoutPosition.minus)) then
      raise EConvertError.Create('Invalid turnout position: "'+strs[1]+'" (TPositiveCondition.Parse)');
  finally
    strs.Free();
  end;
end;

function TPositiveCondition.IdStr(): string;
begin
  Result := IntToStr(Self.turnout) + ':' + TBlkTurnout.PositionToStr(Self.position);
end;

function TPositiveCondition.NameStr(): string;
begin
 Result := Self.proxy.Block(Self.turnout).name + ' : ' + TBlkTurnout.PositionToStr(Self.position);
end;

////////////////////////////////////////////////////////////////////////////////

constructor TPositiveRule.Create();
begin
  inherited;
  Self.conditions := TObjectList<TPositiveCondition>.Create();
  Self.blocks := TList<Integer>.Create();
  Self.blockProxies := TObjectList<TBlockProxy>.Create();
end;

constructor TPositiveRule.Create(line: string);
begin
  Self.Create();
  Self.Parse(line);
end;

destructor TPositiveRule.Destroy();
begin
  Self.conditions.Free();
  Self.blocks.Free();
  Self.blockProxies.Free();
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPositiveRule.Parse(line: string);
begin
  Self.conditions.Clear();
  Self.blocks.Clear();

  var top: TStrings := TStringList.Create();
  try
    ExtractStringsEx(['>'], [' '], line, top);
    if (top.Count = 2) then
    begin
      Self.ParseConditions(top[0]);
      Self.ParseBlocks(top[1]);
    end else if (top.Count = 1) then
    begin
      Self.ParseBlocks(top[0]);
    end else
      raise EConvertError.Create('TPositiveRule.Parse: invalid number of ">" blocks!');
  finally
    top.Free();
  end;
end;

procedure TPositiveRule.ParseConditions(conds: string);
begin
  Self.conditions.Clear();

  var strs: TStrings := TStringList.Create();
  try
    ExtractStringsEx([','], [' '], conds, strs);
    for var str in strs do
      Self.conditions.Add(TPositiveCondition.Create(str));
  finally
    strs.Free();
  end;
end;

procedure TPositiveRule.ParseBlocks(blocks: string);
begin
  Self.blocks.Clear();

  var strs: TStrings := TStringList.Create();
  try
    ExtractStringsEx([','], [' '], blocks, strs);
    for var str in strs do
      Self.blocks.Add(StrToInt(str));
  finally
    strs.Free();
  end;
end;

function TPositiveRule.IdStr(): string;
begin
  Result := '';
  for var cond in Self.conditions do
    Result := Result + cond.IdStr() + ',';
  Result := LeftStr(Result, Length(Result)-1);
  Result := Result + ' > ' + ownConvert.SerializeIntList(Self.blocks);
end;

function TPositiveRule.NameStr(): string;
begin
  Result := '';
  for var cond in Self.conditions do
    Result := Result + cond.NameStr() + ', ';
  Result := LeftStr(Result, Length(Result)-2);
  Result := Result + ' > ';

  for var blockId: Integer in Self.blocks do
    Result := Result + BlockDb.Blocks.GetBlkName(blockId) + ', ';
  Result := LeftStr(Result, Length(Result)-2);
end;

////////////////////////////////////////////////////////////////////////////////

function TPositiveRule.PositiveOn(): Boolean;
begin
  Result := (not Self.ConditionsMet()) or (Self.BlocksPositiveOn());
end;

function TPositiveRule.ConditionsMet(): Boolean;
begin
  Result := True;
  for var condition: TPositiveCondition in Self.conditions do
    if (not condition.Met()) then
      Exit(False);
end;

function TPositiveRule.BlocksPositiveOn(): Boolean;
begin
  Result := True;

  if (Self.blocks.Count <> Self.blockProxies.Count) then
    Self.blockProxies.Clear();

  for var i: Integer := 0 to Self.blocks.Count-1 do
  begin
    if (i >= Self.blockProxies.Count) then
      Self.blockProxies.Add(TBlockProxy.Create());
    var blk: TBlk := Self.blockProxies[i].Block(Self.blocks[i]);
    if (not Self.BlockPositiveOn(blk)) then
      Exit(False);
  end;
end;

function TPositiveRule.BlockPositiveOn(blk: TBlk): Boolean;
begin
  case (blk.typ) of
    TBlkType.btTrack, TBlkType.btRT: begin
      Result := (TBlkTrack(blk).occupied = TTrackState.free);
    end;

    TBlkType.btSignal: begin
      Result := ((TBlkSignal(blk).targetSignal = TBlkSignalCode.ncStuj) and (TBlkSignal(blk).signal = TBlkSignalCode.ncStuj));
    end;
  else
    Result := False;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function PositiveOn(rules: TPositiveRules): Boolean;
begin
  Result := True;
  for var rule: TPositiveRule in rules do
    if (not rule.PositiveOn()) then
      Exit(False);
end;

////////////////////////////////////////////////////////////////////////////////

end.
