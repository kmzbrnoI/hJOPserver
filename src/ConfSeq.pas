unit ConfSeq;

interface

uses Generics.Collections;

type
  TConfSeqItem = record
    target: string;
    condition: string;
  end;

  TConfSeqItems = TList<TConfSeqItem>;

  function CSCondition(block: TObject; condition: string = ''): TConfSeqItem; overload;
  function CSCondition(target: string; condition: string = ''): TConfSeqItem; overload;
  function CSConditions(condition: TConfSeqItem): TConfSeqItems;

implementation

uses Block;

function CSCondition(block: TObject; condition: string): TConfSeqItem;
begin
  Result.target := TBlk(block).name;
  Result.condition := condition;
end;

function CSCondition(target: string; condition: string): TConfSeqItem;
begin
  Result.target := target;
  Result.condition := condition;
end;

function CSConditions(condition: TConfSeqItem): TConfSeqItems;
begin
  Result := TList<TConfSeqItem>.Create();
  Result.Add(condition);
end;

end.
