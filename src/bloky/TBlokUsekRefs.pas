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
   function GetBlockState():TUsekStav;
   function GetChanged():boolean;

 public
  parts: TObjectList<TBlkUsekRef>;
  stateOld: TUsekStav;

   constructor Create(); overload;
   constructor Create(str: string); overload;
   destructor Destroy(); override;

   procedure Parse(str: string);
   function ToStr(): string;

   property state: TUsekStav read GetBlockState;
   property changed: boolean read GetChanged;

end;

implementation

uses ownStrUtils;

////////////////////////////////////////////////////////////////////////////////

constructor TBlkUsekRefs.Create();
begin
 inherited;
 Self.parts := TObjectList<TBlkUsekRef>.Create();
end;

constructor TBlkUsekRefs.Create(str: string);
begin
 inherited Create();
 Self.parts := TObjectList<TBlkUsekRef>.Create();
 Self.Parse(str);
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

function TBlkUsekRefs.GetBlockState():TUsekStav;
var ref: TBlkUsekRef;
begin
 for ref in Self.parts do
  begin
   if (ref.state <> TUsekStav.uvolneno) then
    begin
     Self.stateOld := ref.state;
     Exit(ref.state);
    end;
  end;
 Self.stateOld := TUsekStav.uvolneno;
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

function TBlkUsekRefs.GetChanged():boolean;
var old: TUsekStav;
begin
 old := Self.stateOld;
 Result := (old <> Self.state);
end;

////////////////////////////////////////////////////////////////////////////////

end.
