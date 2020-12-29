unit TBlockTrackRefs;

{
 This unit implements TBlkUsekRefs class, which holds list of TBlkUsekRef.
}

interface

uses TBlockTrackRef, Generics.Collections, Classes, TBlokUsek, StrUtils;

type

TBlkTrackRefs = class
 const
  _SEPARATOR: Char = ',';

 private
   function GetBlockState(): TUsekStav;
   function GetChanged(): Boolean;

 public
  parts: TObjectList<TBlkTrackRef>;
  stateLast: TUsekStav;

   constructor Create(); overload;
   constructor Create(str: string); overload;
   destructor Destroy(); override;

   procedure Parse(str: string);
   function ToStr(): string;

   property state: TUsekStav read GetBlockState;
   property changed: Boolean read GetChanged;

end;

implementation

uses ownStrUtils;

////////////////////////////////////////////////////////////////////////////////

constructor TBlkTrackRefs.Create();
begin
 inherited;
 Self.parts := TObjectList<TBlkTrackRef>.Create();
 Self.stateLast := TUsekStav.uvolneno;
end;

constructor TBlkTrackRefs.Create(str: string);
begin
 inherited Create();
 Self.parts := TObjectList<TBlkTrackRef>.Create();
 Self.Parse(str);
 Self.stateLast := TUsekStav.uvolneno;
end;

destructor TBlkTrackRefs.Destroy();
begin
 Self.parts.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrackRefs.Parse(str: string);
var strs: TStrings;
    _str: string;
    ref: TBlkTrackRef;
begin
 Self.parts.Clear();

 strs := TStringList.Create();
 try
   ExtractStringsEx([_SEPARATOR], [' '], str, strs);
   for _str in strs do
    begin
     ref := TBlkTrackRef.Create(_str);
     Self.parts.Add(ref);
    end;
 finally
   strs.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrackRefs.GetBlockState(): TUsekStav;
var ref: TBlkTrackRef;
begin
 for ref in Self.parts do
   if (ref.state <> TUsekStav.uvolneno) then
     Exit(ref.state);
 Result := TUsekStav.uvolneno;
end;

function TBlkTrackRefs.ToStr(): string;
var ref: TBlkTrackRef;
begin
 Result := '';
 for ref in Self.parts do
   Result := Result + ref.ToStr() + _SEPARATOR;
 Result := LeftStr(Result, Length(Result)-1);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrackRefs.GetChanged(): Boolean;
begin
 Result := (Self.stateLast <> Self.state);
 Self.stateLast := Self.state;
end;

////////////////////////////////////////////////////////////////////////////////

end.
