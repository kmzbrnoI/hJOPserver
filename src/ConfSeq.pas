unit ConfSeq;

interface

uses Generics.Collections;

type
  TConfSeqItem = record
    target: string;
    note: string;
  end;

  TConfSeqItems = TList<TConfSeqItem>;

  function CSItem(block: TObject; note: string = ''): TConfSeqItem; overload;
  function CSItem(target: string; note: string = ''): TConfSeqItem; overload;
  function CSItems(item: TConfSeqItem): TConfSeqItems;

implementation

uses Block;

function CSItem(block: TObject; note: string): TConfSeqItem;
begin
  Result.target := TBlk(block).name;
  Result.note := note;
end;

function CSItem(target: string; note: string): TConfSeqItem;
begin
  Result.target := target;
  Result.note := note;
end;

function CSItems(item: TConfSeqItem): TConfSeqItems;
begin
  Result := TList<TConfSeqItem>.Create();
  Result.Add(item);
end;

end.
