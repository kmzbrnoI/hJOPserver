unit BlockProxy;

// Block-proxy is a usually-O(1) way to get instance of block based on its id.
// It stores the last-returned block and expects the asked block to be the same.

interface

uses Block;

type
  TBlockProxy = class(TObject)
  private
    _block: TBlk;
  public
     constructor Create();
     function Block(id: Integer): TBlk;
  end;

implementation

uses BlockDb;

constructor TBlockProxy.Create();
begin
  Self._block := nil;
end;

function TBlockProxy.Block(id: Integer): TBlk;
begin
  if ((Self._block = nil) or (Self._block.id <> id)) then
    Self._block := Blocks.GetBlkByID(id);
  Result := Self._block;
end;

end.
