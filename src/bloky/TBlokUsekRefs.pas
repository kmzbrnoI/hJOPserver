unit TBlokUsekRefs;

{
 This unit implements TBlkUsekRefs class, which holds list of TBlkUsekRef.
}

interface

uses TBlokUsekRef, Generics.Collections, Classes, TBlokUsek, StrUtils;

type

TBlkUsekRefs = class
 const
  _SEPARATOR: Char = ',';

 private
   function GetBlockState(): TUsekStav;
   function GetChanged(): Boolean;

 public
  parts: TObjectList<TBlkUsekRef>;
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

constructor TBlkUsekRefs.Create();
begin
 inherited;
 Self.parts := TObjectList<TBlkUsekRef>.Create();
 Self.stateLast := TUsekStav.uvolneno;
end;

constructor TBlkUsekRefs.Create(str: string);
begin
 inherited Create();
 Self.parts := TObjectList<TBlkUsekRef>.Create();
 Self.Parse(str);
 Self.stateLast := TUsekStav.uvolneno;
end;

destructor TBlkUsekRefs.Destroy();
begin
 Self.parts.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsekRefs.Parse(str: string);
var strs: TStrings;
    _str: string;
    ref: TBlkUsekRef;
begin
 Self.parts.Clear();

 strs := TStringList.Create();
 try
   ExtractStringsEx([_SEPARATOR], [' '], str, strs);
   for _str in strs do
    begin
     ref := TBlkUsekRef.Create(_str);
     Self.parts.Add(ref);
    end;
 finally
   strs.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsekRefs.GetBlockState(): TUsekStav;
var ref: TBlkUsekRef;
begin
 for ref in Self.parts do
   if (ref.state <> TUsekStav.uvolneno) then
     Exit(ref.state);
 Result := TUsekStav.uvolneno;
end;

function TBlkUsekRefs.ToStr(): string;
var ref: TBlkUsekRef;
begin
 Result := '';
 for ref in Self.parts do
   Result := Result + ref.ToStr() + _SEPARATOR;
 Result := LeftStr(Result, Length(Result)-1);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsekRefs.GetChanged(): Boolean;
begin
 Result := (Self.stateLast <> Self.state);
 Self.stateLast := Self.state;
end;

////////////////////////////////////////////////////////////////////////////////

end.
