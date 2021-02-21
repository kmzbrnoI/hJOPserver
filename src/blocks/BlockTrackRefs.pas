unit BlockTrackRefs;

{
  This unit implements TBlkUsekRefs class, which holds list of TBlkUsekRef.
}

interface

uses BlockTrackRef, Generics.Collections, Classes, BlockTrack, StrUtils;

type

  TBlkTrackRefs = class
  const
    _SEPARATOR: Char = ',';

  private
    function GetBlockState(): TTrackState;
    function GetChanged(): Boolean;

  public
    parts: TObjectList<TBlkTrackRef>;
    stateLast: TTrackState;

    constructor Create(); overload;
    constructor Create(str: string); overload;
    destructor Destroy(); override;

    procedure Parse(str: string);
    function ToStr(): string;

    property state: TTrackState read GetBlockState;
    property changed: Boolean read GetChanged;

  end;

implementation

uses ownStrUtils;

/// /////////////////////////////////////////////////////////////////////////////

constructor TBlkTrackRefs.Create();
begin
  inherited;
  Self.parts := TObjectList<TBlkTrackRef>.Create();
  Self.stateLast := TTrackState.free;
end;

constructor TBlkTrackRefs.Create(str: string);
begin
  inherited Create();
  Self.parts := TObjectList<TBlkTrackRef>.Create();
  Self.Parse(str);
  Self.stateLast := TTrackState.free;
end;

destructor TBlkTrackRefs.Destroy();
begin
  Self.parts.free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

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
    strs.free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrackRefs.GetBlockState(): TTrackState;
begin
  for var ref: TBlkTrackRef in Self.parts do
    if (ref.state <> TTrackState.free) then
      Exit(ref.state);
  Result := TTrackState.free;
end;

function TBlkTrackRefs.ToStr(): string;
begin
  Result := '';
  for var ref: TBlkTrackRef in Self.parts do
    Result := Result + ref.ToStr() + _SEPARATOR;
  Result := LeftStr(Result, Length(Result) - 1);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTrackRefs.GetChanged(): Boolean;
begin
  Result := (Self.stateLast <> Self.state);
  Self.stateLast := Self.state;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
