unit BlockTrackRef;

{
  This unit implements TBlkUsekRef class, which holds reference to a section
  (usek) or a section part.
}

interface

uses Block, BlockTrack, Classes, SysUtils, Generics.Collections;

type

  ENoBlock = class(Exception);
  EInvalidPart = class(Exception);

  TBlkTrackRef = class
  const
    _EMPTY_PARTID: Integer = -1;
    _EMPTY_BLOCKID: Integer = -1;
    _SEPARATOR: Char = ':';

  private
    function MIsPart(): Boolean;
    procedure Parse(str: string);
    function GetBlock(): TBlkTrack;
    function GetBlockState(): TTrackState;

  public
    blockId: Integer;
    partId: Integer;

    constructor Create(blockId: Integer); overload;
    constructor Create(blockId: Integer; partId: Integer); overload;
    constructor Create(str: string); overload;
    function ToStr(): string;
    procedure AddOccupiedTracksId(var occupied: TList<Integer>);

    property isPart: Boolean read MIsPart;
    property Block: TBlkTrack read GetBlock;
    property state: TTrackState read GetBlockState;

  end;

implementation

uses BlockDb, ownStrUtils;

/// /////////////////////////////////////////////////////////////////////////////

constructor TBlkTrackRef.Create(blockId: Integer);
begin
  inherited Create();
  Self.blockId := blockId;
  Self.partId := _EMPTY_PARTID;
end;

constructor TBlkTrackRef.Create(blockId: Integer; partId: Integer);
begin
  inherited Create();
  Self.blockId := blockId;
  Self.partId := partId;
end;

constructor TBlkTrackRef.Create(str: string);
begin
  inherited Create();
  Self.Parse(str);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrackRef.Parse(str: string);
var strs: TStrings;
begin
  strs := TStringList.Create();
  try
    ExtractStringsEx([_SEPARATOR], [' '], str, strs);
    if (strs.Count > 0) then
      Self.blockId := StrToInt(strs[0])
    else
      Self.blockId := _EMPTY_BLOCKID;
    if (strs.Count > 1) then
      Self.partId := StrToInt(strs[1])
    else
      Self.partId := _EMPTY_PARTID;
  finally
    strs.Free();
  end;
end;

function TBlkTrackRef.MIsPart(): Boolean;
begin
  Result := (Self.partId <> _EMPTY_PARTID);
end;

function TBlkTrackRef.ToStr(): string;
begin
  Result := '';
  if (Self.blockId <> _EMPTY_BLOCKID) then
  begin
    Result := Result + IntToStr(Self.blockId);
    if (Self.partId <> _EMPTY_PARTID) then
      Result := Result + _SEPARATOR + IntToStr(Self.partId);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrackRef.GetBlock(): TBlkTrack;
begin
  Result := Blocks.GetBlkTrackOrRTByID(Self.blockId);
end;

function TBlkTrackRef.GetBlockState(): TTrackState;
begin
  if (Self.Block = nil) then
    raise ENoBlock.Create('No block, unable to get state!');

  if (Self.isPart) then
  begin
    if ((Self.partId < 0) or (Self.partId >= Self.Block.sectionsState.Count)) then
      raise EInvalidPart.Create('Invalid part ' + IntToStr(Self.partId));
    Result := Self.Block.sectionsState[Self.partId];
  end
  else
    Result := Self.Block.occupied;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTrackRef.AddOccupiedTracksId(var occupied: TList<Integer>);
begin
  if ((Self.state = TTrackState.occupied) and (not occupied.Contains(Self.blockId))) then
    occupied.Add(Self.blockId);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
