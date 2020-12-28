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
   procedure RemoveUsekNeprofil(Sender: TObject; data: Integer);
   procedure RemoveEvent(Sender: TObject; data: Integer);
 end;

var ceCaller: TChangeEventCaller;

implementation

uses TBloky, TBlok, TBlokUsek, TBlokVyhybka, TBlokZamek, TBlokPrejezd,
     TBlokTrat;

////////////////////////////////////////////////////////////////////////////////

procedure TChangeEventCaller.CopyUsekZaver(Sender: TObject; data: Integer);
var blk: TBlk;
begin
 Blky.GetBlkByID(data, blk);
 if ((blk = nil) or ((blk.typ <> btUsek) and
    (blk.typ <> btTU))) then Exit();

 TBlkUsek(Blk).Zaver := TBlkUsek(Sender).Zaver;
end;

procedure TChangeEventCaller.NullZamekZaver(Sender: TObject; data: Integer);
var blk: TBlk;
begin
 Blky.GetBlkByID(data, blk);
 if ((blk = nil) or (blk.typ <> btLock)) then Exit();

 TBlkLock(Blk).Zaver := false;
end;

procedure TChangeEventCaller.NullPrejezdZaver(Sender: TObject; data: Integer);
var blk: TBlk;
begin
 Blky.GetBlkByID(data, blk);
 if ((blk = nil) or (blk.typ <> btPrejezd)) then Exit();

 TBlkPrejezd(Blk).Zaver := false;
end;

procedure TChangeEventCaller.NullTratZaver(Sender: TObject; data: Integer);
var blk: TBlk;
begin
 Blky.GetBlkByID(data, blk);
 if ((blk = nil) or (blk.typ <> btTrat)) then Exit();

 TBlkTrat(Blk).Zaver := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TChangeEventCaller.NullVyhybkaMenuReduction(Sender: TObject; data: Integer);
var blk: TBlk;
begin
 Blky.GetBlkByID(data, blk);
 if ((blk = nil) or (blk.typ <> btTurnout)) then Exit();

 TBlkTurnout(Blk).IntentionalUnlock();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TChangeEventCaller.RemoveUsekNeprofil(Sender: TObject; data: Integer);
var blk: TBlk;
    caller:^TNPCallerData;
begin
 caller := Pointer(data);

 Blky.GetBlkByID(caller.usekId, blk);
 if ((blk = nil) or ((blk.typ <> btUsek) and (blk.typ <> btTU))) then Exit();

 TBlkUsek(Blk).RemoveNeprofilJC(caller.jcId);
 FreeMem(caller);
end;

////////////////////////////////////////////////////////////////////////////////

constructor TRemoveEventData.Create(events: TChangeEvents; event: TChangeEvent);
begin
 Self.events := events;
 Self.event := event;
end;

procedure TChangeEventCaller.RemoveEvent(Sender: TObject; data: Integer);
var event: ^TRemoveEventData;
begin
 event := Pointer(data);

 if (event.events.Contains(event.event)) then
   event.events.Remove(event.event);

 FreeMem(event);
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  ceCaller := TChangeEventCaller.Create();
finalization
  ceCaller.Free();

end.
