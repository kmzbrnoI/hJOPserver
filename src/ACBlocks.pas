unit ACBlocks;

{ TACBlk class manages AC PanelServer clients. It informs them about block changes
  and sends them AC state notifications. }

interface

uses Generics.Collections, IdContext, Classes, AnsiStrings, SysUtils;

type
  TACBlk = class
  private
    blkToClients: TObjectDictionary<Integer, TList<TIdContext>>;
    clientToBlks: TObjectDictionary<Pointer, TList<Integer>>;

  public

    constructor Create();
    destructor Destroy(); override;

    procedure ClientAddBlock(client: TIdContext; blkid: Integer);
    procedure ClientRemoveBlock(client: TIdContext; blkid: Integer);

    procedure ParseBlocksMessage(Sender: TIdContext; parsed: TStrings);
    procedure OnBlkChange(blkid: Integer);
    procedure OnClientDisconnect(client: TIdContext);
    procedure RemoveAllClients();

  end;

var
  ACBlk: TACBlk;

implementation

uses ownStrUtils, TCPServerPanel, BlockDb;

/// /////////////////////////////////////////////////////////////////////////////

constructor TACBlk.Create();
begin
  inherited;
  Self.blkToClients := TObjectDictionary < Integer, TList < TIdContext >> .Create([doOwnsValues]);
  Self.clientToBlks := TObjectDictionary < Pointer, TList < Integer >>.Create([doOwnsValues]);
end;

destructor TACBlk.Destroy();
begin
  Self.blkToClients.Free();
  Self.clientToBlks.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TACBlk.ClientAddBlock(client: TIdContext; blkid: Integer);
begin
  if (not clientToBlks.ContainsKey(client)) then
    Self.clientToBlks.Add(client, TList<Integer>.Create());
  if (not blkToClients.ContainsKey(blkid)) then
    Self.blkToClients.Add(blkid, TList<TIdContext>.Create());

  if (not Self.clientToBlks[client].Contains(blkid)) then
    Self.clientToBlks[client].Add(blkid);
  if (not Self.blkToClients[blkid].Contains(client)) then
    Self.blkToClients[blkid].Add(client);
end;

procedure TACBlk.ClientRemoveBlock(client: TIdContext; blkid: Integer);
begin
  if (Self.clientToBlks.ContainsKey(client)) then
    Self.clientToBlks[client].Remove(blkid);
  if (Self.blkToClients.ContainsKey(blkid)) then
    Self.blkToClients[blkid].Remove(client);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TACBlk.ParseBlocksMessage(Sender: TIdContext; parsed: TStrings);
var
  blks: TStrings;
  blk, list: string;
  blkid: Integer;
begin
  if ((parsed.Count >= 5) and (UpperCase(parsed[3]) = 'BLOCKS')) then
  begin
    if ((parsed.Count >= 6) and (UpperCase(parsed[4]) = 'REGISTER')) then
    begin
      blks := TStringList.Create();
      try
        ExtractStringsEx([','], [], parsed[5], blks);
        for blk in blks do
        begin
          try
            blkid := StrToInt(blk);
            if (not Blocks.IsBlok(blkid)) then
              raise Exception.Create('Blok ' + blk + ' neexistuje');
            Self.ClientAddBlock(Sender, blkid);
            PanelServer.SendLn(Sender, '-;AC;-;BLOCKS;REGISTER;' + blk + ';OK');
          except
            on E: Exception do
              PanelServer.SendLn(Sender, '-;AC;-;BLOCKS;REGISTER;' + blk + ';ERR;' + E.Message);
          end;
        end;
      finally
        blks.Free();
      end;
    end else if ((parsed.Count >= 6) and (UpperCase(parsed[4]) = 'UNREGISTER')) then
    begin
      blks := TStringList.Create();
      try
        ExtractStringsEx([','], [], parsed[5], blks);
        for blk in blks do
        begin
          try
            blkid := StrToInt(blk);
            Self.ClientRemoveBlock(Sender, blkid);
            PanelServer.SendLn(Sender, '-;AC;-;BLOCKS;UNREGISTER;' + blk + ';OK');
          except
            on E: Exception do
              PanelServer.SendLn(Sender, '-;AC;-;BLOCKS;UNREGISTER;' + blk + ';ERR;' + E.Message);
          end;
        end;
      finally
        blks.Free();
      end;
    end else if (UpperCase(parsed[4]) = 'LIST') then
    begin
      list := '';
      if (Self.clientToBlks.ContainsKey(Sender)) then
        for blkid in Self.clientToBlks[Sender] do
          list := list + IntToStr(blkid) + ',';
      PanelServer.SendLn(Sender, '-;AC;-;BLOCKS;LIST;{' + list + '}');
    end;

  end;
end;

procedure TACBlk.OnBlkChange(blkid: Integer);
var
  client: TIdContext;
begin
  if (not Self.blkToClients.ContainsKey(blkid)) then
    Exit();
  for client in Self.blkToClients[blkid] do
    PanelServer.SendLn(client, '-;AC;-;BLOCKS;CHANGE;' + IntToStr(blkid));
end;

procedure TACBlk.OnClientDisconnect(client: TIdContext);
var
  blkid: Integer;
begin
  if (not Self.clientToBlks.ContainsKey(client)) then
    Exit();
  for blkid in Self.clientToBlks[client] do
  begin
    if (Self.blkToClients.ContainsKey(blkid)) then
      Self.blkToClients[blkid].Remove(client);
  end;
  Self.clientToBlks.Remove(client);
end;

procedure TACBlk.RemoveAllClients();
begin
  Self.blkToClients.Clear();
  Self.clientToBlks.Clear();
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

ACBlk := TACBlk.Create();

finalization

ACBlk.Free();

end.
