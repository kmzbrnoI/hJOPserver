unit TechnologieAB;

{
  Trida TABlist udrzuje seznam automaticky stavenych jizdnich cest a postupne
  se pokousi je stavet.
}

interface

uses SysUtils, Generics.Collections, TechnologieJC, JCBarriers;

type
  EABJCAlreadyInList = class(Exception);
  EABJCNotInList = class(Exception);

  TABlist = class
  private
    JCs: TList<TJC>;

    function GetItem(index: Integer): TJC;
    procedure TryJC(jc: TJC);

  public

    constructor Create();
    destructor Destroy(); override;

    procedure Add(jc: TJC);
    procedure Remove(jc: TJC);
    function Contains(jc: TJC): Boolean;

    procedure Update();

    function IsTrackInAnyABJC(trackId: Integer): Boolean;

    property Items[index: Integer]: TJC read GetItem; default;
  end;

var
  ABlist: TABlist;

implementation

uses DataAB, Block, BlockDb, BlockTrack, logging, BlockSignal;

/// /////////////////////////////////////////////////////////////////////////////

constructor TABlist.Create();
begin
  inherited;
  Self.JCs := TList<TJC>.Create();
end;

destructor TABlist.Destroy();
begin
  Self.JCs.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TABlist.Add(jc: TJC);
begin
  if (Self.JCs.Contains(jc)) then
    raise EABJCAlreadyInList.Create('This JC is already in AB list!');

  Self.JCs.Add(jc);
  ABTableData.AddJC(jc);
  Log('AB: JC ' + jc.name + ' přidána do seznamu AB JC', llInfo, lsJC);
end;

procedure TABlist.Remove(jc: TJC);
begin
  if (not Self.JCs.Contains(jc)) then
    raise EABJCNotInList.Create('This JC is not in AB list!');

  for var trackId: Integer in jc.data.tracks do
  begin
    var track: TBlkTrack := Blocks.GetBlkTrackOrRTByID(trackId);
    if ((track <> nil) and (track.zaver = TZaver.ab)) then
      track.zaver := TZaver.no;
  end;

  var i: Integer := Self.JCs.IndexOf(jc);
  Log('AB: JC ' + Self.JCs[i].name + ' odstraněna ze seznamu AB JC', llInfo, lsJC);
  Self.JCs.Delete(i);
  ABTableData.DeleteJC(i);
end;

function TABlist.Contains(jc: TJC): Boolean;
begin
  Result := Self.JCs.Contains(jc);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TABlist.TryJC(jc: TJC);
var barriers: TJCBarriers;
begin
  barriers := jc.Barriers();
  try
    // v jizdni ceste jsou urcite bariery (musi tam byt minimalne zavery AB cesty)
    for var barrier: TJCBarrier in barriers do
    begin
      if ((barrier.typ <> barTrackAB) and ((JCBarriers.CriticalBarrier(barrier.typ)) or
        (not JCBarriers.JCWarningBarrier(barrier.typ)))) then
        Exit();
    end;

    // Tady mame zajisteno, ze v jizdni ceste nejsou kriticke ani nevarovne bariery
    // (KontrolaPodminek() zarucuje, ze tyto typy barier jsou na zacatku seznamu).
    // Upozornovaci bariery ignorujeme a stavime JC.

    Log('DN JC ' + jc.name + ' : podmínky splněny, stavím', llInfo, lsStack);

    var signal: TBlkSignal := Blocks.GetBlkSignalByID(jc.data.signalId);
    if ((signal = nil) or (signal.areas.Count = 0)) then
      Self.Remove(jc);

    jc.Activate(nil, signal.areas[0], nil, false, true);
  finally
    barriers.Free();
  end;
end;

// Zkousi stavet kazdou z jizdnich cest v seznamu Self.JCs.
procedure TABlist.Update();
begin
  for var jc: TJC in Self.JCs do
  begin
    if ((jc.active) or (jc.activating)) then
      continue;
    Self.TryJC(jc);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TABlist.GetItem(index: Integer): TJC;
begin
  Result := Self.JCs[index];
end;

/// /////////////////////////////////////////////////////////////////////////////

function TABlist.IsTrackInAnyABJC(trackId: Integer): Boolean;
begin
  for var jc: TJC in Self.JCs do
    for var id: Integer in jc.data.tracks do
      if (id = trackId) then
        Exit(true);

  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

ABlist := TABlist.Create();

finalization

FreeAndNil(ABlist);

end.
