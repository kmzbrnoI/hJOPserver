unit TBlockTrackRef;

{
 This unit implements TBlkUsekRef class, which holds reference to a section
 (usek) or a section part.
}

interface

uses TBlok, TBlokUsek, Classes, SysUtils;

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
   function GetBlock(): TBlkUsek;
   function GetBlockState(): TUsekStav;

 public
  blockId: Integer;
  partId: Integer;

   constructor Create(blockId: Integer); overload;
   constructor Create(blockId: Integer; partId: Integer); overload;
   constructor Create(str: string); overload;
   function ToStr(): string;

   property isPart: Boolean read MIsPart;
   property block: TBlkUsek read GetBlock;
   property state: TUsekStav read GetBlockState;

end;

implementation

uses TBloky, ownStrUtils;

////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////

function TBlkTrackRef.GetBlock(): TBlkUsek;
var blk: TBlk;
begin
 Blky.GetBlkById(Self.blockId, blk);
 if ((blk = nil) or ((blk.typ = btUsek) or (blk.typ = btTU))) then
   Result := TBlkUsek(blk)
 else
   Result := nil;
end;

function TBlkTrackRef.GetBlockState(): TUsekStav;
begin
 if (Self.block = nil) then
   raise ENoBlock.Create('No block, unable to get state!');

 if (Self.isPart) then
  begin
   if ((Self.partId < 0) or (Self.partId >= Self.block.SekceStav.Count)) then
     raise EInvalidPart.Create('Invalid part '+IntToStr(Self.partId));
   Result := Self.block.SekceStav[Self.partId];
  end else
   Result := Self.block.Obsazeno;
end;

////////////////////////////////////////////////////////////////////////////////

end.
