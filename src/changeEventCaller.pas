unit changeEventCaller;

{
  Trida TChangeEventCaller poskutuje pomocne funkce pro volani ChangeEvents
  (viz changeEvent.pas).
}

interface

uses changeEvent;

type
  TNPCallerData = record
    usekId: Integer;
    jcId: Integer;
  end;

  TRemoveEventData = record
    events: TChangeEvents; // object reference
    event: TChangeEvent;
    constructor Create(events: TChangeEvents; event: TChangeEvent);
  end;

  TChangeEventCaller = class
    procedure CopyTrackZaver(Sender: TObject; data: Integer);
    procedure LockCancelZaver(Sender: TObject; data: Integer);
    procedure CrossingCancelZaver(Sender: TObject; data: Integer);
    procedure RailwayCancelZaver(Sender: TObject; data: Integer);
    procedure TurnoutUnlock(Sender: TObject; data: Integer);
    procedure RemoveUsekNeprofil(Sender: TObject; data: Pointer);
    procedure RemoveEvent(Sender: TObject; data: Pointer);
  end;

var ceCaller: TChangeEventCaller;

implementation

uses BlockDb, Block, BlockTrack, BlockTurnout, BlockLock, BlockCrossing,
  BlockRailway;

/// /////////////////////////////////////////////////////////////////////////////

procedure TChangeEventCaller.CopyTrackZaver(Sender: TObject; data: Integer);
begin
  var track := Blocks.GetBlkTrackOrRTByID(data);
  if (track = nil) then
    Exit();

  track.zaver := TBlkTrack(Sender).zaver;
end;

procedure TChangeEventCaller.LockCancelZaver(Sender: TObject; data: Integer);
begin
  var lock: TBlkLock := Blocks.GetBlkLockByID(data);
  if (lock = nil) then
    Exit();
  lock.zaver := false;
end;

procedure TChangeEventCaller.CrossingCancelZaver(Sender: TObject; data: Integer);
begin
  var crossing := Blocks.GetBlkCrossingByID(data);
  if (crossing = nil) then
    Exit();
  crossing.zaver := false;
end;

procedure TChangeEventCaller.RailwayCancelZaver(Sender: TObject; data: Integer);
begin
  var railway: TBlkRailway := Blocks.GetBlkRailwayByID(data);
  if (railway = nil) then
    Exit();
  railway.Zaver := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TChangeEventCaller.TurnoutUnlock(Sender: TObject; data: Integer);
begin
  var turnout := Blocks.GetBlkTurnoutByID(data);
  if (turnout = nil) then
    Exit();

  turnout.IntentionalUnlock();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TChangeEventCaller.RemoveUsekNeprofil(Sender: TObject; data: Pointer);
var caller: ^TNPCallerData;
begin
  caller := data;

  var track := Blocks.GetBlkTrackOrRTByID(caller^.usekId);
  if (track = nil) then
    Exit();

  track.RemoveNeprofilJC(caller^.jcId);
  FreeMem(caller);
end;

/// /////////////////////////////////////////////////////////////////////////////

constructor TRemoveEventData.Create(events: TChangeEvents; event: TChangeEvent);
begin
  Self.events := events;
  Self.event := event;
end;

procedure TChangeEventCaller.RemoveEvent(Sender: TObject; data: Pointer);
var event: ^TRemoveEventData;
begin
  event := data;

  if (event^.events.Contains(event.event)) then
    event^.events.Remove(event.event);

  FreeMem(event);
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

ceCaller := TChangeEventCaller.Create();

finalization

ceCaller.Free();

end.
