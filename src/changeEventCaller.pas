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

  TCECaller = class
    class procedure CopyTrackZaver(Sender: TObject; data: Integer);
    class procedure LockCancelZaver(Sender: TObject; data: Integer);
    class procedure CrossingCancelZaver(Sender: TObject; data: Integer);
    class procedure RailwayCancelZaver(Sender: TObject; data: Integer);
    class procedure TurnoutRefugeeUnlock(Sender: TObject; data: Integer);
    class procedure IOCancelZaver(Sender: TObject; data: Integer);
    class procedure RemoveUsekNeprofil(Sender: TObject; data: Pointer);
    class procedure RemoveEvent(Sender: TObject; data: Pointer);
  end;

implementation

uses BlockDb, Block, BlockTrack, BlockTurnout, BlockLock, BlockCrossing,
  BlockRailway, BlockIO;

/// /////////////////////////////////////////////////////////////////////////////

class procedure TCECaller.CopyTrackZaver(Sender: TObject; data: Integer);
begin
  var track := Blocks.GetBlkTrackOrRTByID(data);
  if (track <> nil) then
    track.zaver := TBlkTrack(Sender).zaver;
end;

class procedure TCECaller.LockCancelZaver(Sender: TObject; data: Integer);
begin
  var lock: TBlkLock := Blocks.GetBlkLockByID(data);
  if (lock <> nil) then
    lock.RemovePathZaver(TBlk(Sender).id);
end;

class procedure TCECaller.IOCancelZaver(Sender: TObject; data: Integer);
begin
  var io: TBlkIO := Blocks.GetBlkIOByID(data);
  if (io <> nil) then
    io.inZaver := False;
end;
class
procedure TCECaller.CrossingCancelZaver(Sender: TObject; data: Integer);
begin
  var crossing := Blocks.GetBlkCrossingByID(data);
  if (crossing <> nil) then
    crossing.zaver := False;
end;

class procedure TCECaller.RailwayCancelZaver(Sender: TObject; data: Integer);
begin
  var railway: TBlkRailway := Blocks.GetBlkRailwayByID(data);
  if (railway <> nil) then
    railway.Zaver := False;
end;

class procedure TCECaller.TurnoutRefugeeUnlock(Sender: TObject; data: Integer);
begin
  var turnout := Blocks.GetBlkTurnoutByID(data);
  if (turnout <> nil) then
    turnout.RefugeeUnlock(TBlk(Sender).id);
end;

class procedure TCECaller.RemoveUsekNeprofil(Sender: TObject; data: Pointer);
var caller: ^TNPCallerData;
begin
  caller := data;

  var track := Blocks.GetBlkTrackOrRTByID(caller^.usekId);
  if (track <> nil) then
  begin
    track.RemoveNeprofilJC(caller^.jcId);
    FreeMem(caller);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

constructor TRemoveEventData.Create(events: TChangeEvents; event: TChangeEvent);
begin
  Self.events := events;
  Self.event := event;
end;

class procedure TCECaller.RemoveEvent(Sender: TObject; data: Pointer);
var event: ^TRemoveEventData;
begin
  event := data;

  if (event^.events.Contains(event.event)) then
    event^.events.Remove(event.event);

  FreeMem(event);
end;

end.
