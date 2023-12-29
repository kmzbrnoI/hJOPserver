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
    procedure CopyUsekZaver(Sender: TObject; data: Integer);
    procedure NullZamekZaver(Sender: TObject; data: Integer);
    procedure NullPrejezdZaver(Sender: TObject; data: Integer);
    procedure NullTratZaver(Sender: TObject; data: Integer);
    procedure NullVyhybkaMenuReduction(Sender: TObject; data: Integer);
    procedure RemoveUsekNeprofil(Sender: TObject; data: Pointer);
    procedure RemoveEvent(Sender: TObject; data: Pointer);
  end;

var ceCaller: TChangeEventCaller;

implementation

uses BlockDb, Block, BlockTrack, BlockTurnout, BlockLock, BlockCrossing,
  BlockRailway;

/// /////////////////////////////////////////////////////////////////////////////

procedure TChangeEventCaller.CopyUsekZaver(Sender: TObject; data: Integer);
begin
  var track := Blocks.GetBlkTrackOrRTByID(data);
  if (track = nil) then
    Exit();

  track.zaver := TBlkTrack(Sender).zaver;
end;

procedure TChangeEventCaller.NullZamekZaver(Sender: TObject; data: Integer);
begin
  var lock: TBlkLock := Blocks.GetBlkLockByID(data);
  if (lock = nil) then
    Exit();
  lock.Zaver := false;
end;

procedure TChangeEventCaller.NullPrejezdZaver(Sender: TObject; data: Integer);
begin
  var crossing := Blocks.GetBlkCrossingByID(data);
  if (crossing = nil) then
    Exit();
  crossing.Zaver := false;
end;

procedure TChangeEventCaller.NullTratZaver(Sender: TObject; data: Integer);
begin
  var railway: TBlkRailway := Blocks.GetBlkRailwayByID(data);
  if (railway = nil) then
    Exit();
  railway.Zaver := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TChangeEventCaller.NullVyhybkaMenuReduction(Sender: TObject; data: Integer);
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
