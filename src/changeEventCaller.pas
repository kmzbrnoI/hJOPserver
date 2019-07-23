unit changeEventCaller;

{
  Trida TChangeEventCaller poskutuje pomocne funkce pro volani ChangeEvents
  (viz changeEvent.pas).
}

interface

uses changeEvent;

type
 TNPCallerData = record
   usekId:Integer;
   jcId:Integer;
 end;

 TChangeEventCaller = class
   procedure CopyUsekZaver(Sender:TObject; data:Integer);
   procedure NullZamekZaver(Sender:TObject; data:Integer);
   procedure NullPrejezdZaver(Sender:TObject; data:Integer);
   procedure NullTratZaver(Sender:TObject; data:Integer);
   procedure NullVyhybkaMenuReduction(Sender:TObject; data:Integer);
   procedure NullSComMenuReduction(Sender:TObject; data:Integer);
   procedure RemoveUsekNeprofil(Sender:TObject; data:Integer);
 end;

var ceCaller: TChangeEventCaller;

implementation

uses TBloky, TBlok, TBlokUsek, TBlokVyhybka, TBlokZamek, TBlokNav, TBlokPrejezd,
     TBlokTrat;

////////////////////////////////////////////////////////////////////////////////

procedure TChangeEventCaller.CopyUsekZaver(Sender:TObject; data:Integer);
var blk:TBlk;
begin
 Blky.GetBlkByID(data, blk);
 if ((blk = nil) or ((blk.typ <> _BLK_USEK) and
    (blk.typ <> _BLK_TU))) then Exit();

 TBlkUsek(Blk).Zaver := TBlkUsek(Sender).Zaver;
end;

procedure TChangeEventCaller.NullZamekZaver(Sender:TObject; data:Integer);
var blk:TBlk;
begin
 Blky.GetBlkByID(data, blk);
 if ((blk = nil) or (blk.typ <> _BLK_ZAMEK)) then Exit();

 TBlkZamek(Blk).Zaver := false;
end;

procedure TChangeEventCaller.NullPrejezdZaver(Sender:TObject; data:Integer);
var blk:TBlk;
begin
 Blky.GetBlkByID(data, blk);
 if ((blk = nil) or (blk.typ <> _BLK_PREJEZD)) then Exit();

 TBlkPrejezd(Blk).Zaver := false;
end;

procedure TChangeEventCaller.NullTratZaver(Sender:TObject; data:Integer);
var blk:TBlk;
begin
 Blky.GetBlkByID(data, blk);
 if ((blk = nil) or (blk.typ <> _BLK_TRAT)) then Exit();

 TBlkTrat(Blk).Zaver := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TChangeEventCaller.NullVyhybkaMenuReduction(Sender:TObject; data:Integer);
var blk:TBlk;
begin
 Blky.GetBlkByID(data, blk);
 if ((blk = nil) or (blk.typ <> _BLK_VYH)) then Exit();

 TBlkVyhybka(Blk).ZrusRedukciMenu();
end;

procedure TChangeEventCaller.NullSComMenuReduction(Sender:TObject; data:Integer);
var blk:TBlk;
begin
 Blky.GetBlkByID(data, blk);
 if ((blk = nil) or (blk.typ <> _BLK_NAV)) then Exit();

 TBlkNav(Blk).ZrusRedukciMenu();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TChangeEventCaller.RemoveUsekNeprofil(Sender:TObject; data:Integer);
var blk:TBlk;
    caller:^TNPCallerData;
begin
 caller := Pointer(data);

 Blky.GetBlkByID(caller.usekId, blk);
 if ((blk = nil) or ((blk.typ <> _BLK_USEK) and
    (blk.typ <> _BLK_TU))) then Exit();

 TBlkUsek(Blk).RemoveNeprofilJC(caller.jcId);
 FreeMem(caller);
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  ceCaller := TChangeEventCaller.Create();
finalization
  ceCaller.Free();

end.
