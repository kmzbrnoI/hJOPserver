unit changeEventCaller;

{
  Trida TChangeEventCaller poskutuje pomocne funkce pro volani ChangeEvents
  (viz changeEvent.pas).
}

interface

uses changeEvent;

type
 TChangeEventCaller = class
   procedure NullUsekZaver(Sender:TObject; data:Integer);
   procedure NullZamekZaver(Sender:TObject; data:Integer);
   procedure NullPrejezdZaver(Sender:TObject; data:Integer);
   procedure NullTratZaver(Sender:TObject; data:Integer);
   procedure NullVyhybkaMenuReduction(Sender:TObject; data:Integer);
   procedure NullSComMenuReduction(Sender:TObject; data:Integer);
 end;

var ceCaller: TChangeEventCaller;

implementation

uses TBloky, TBlok, TBlokUsek, TBlokVyhybka, TBlokZamek, TBlokSCom, TBlokPrejezd,
     TBlokTrat;

////////////////////////////////////////////////////////////////////////////////

procedure TChangeEventCaller.NullUsekZaver(Sender:TObject; data:Integer);
var blk:TBlk;
begin
 Blky.GetBlkByID(data, blk);
 if ((blk = nil) or ((blk.GetGlobalSettings.typ <> _BLK_USEK) and
    (blk.GetGlobalSettings.typ <> _BLK_TU))) then Exit();

 TBlkUsek(Blk).Zaver := TZaver.no;
end;

procedure TChangeEventCaller.NullZamekZaver(Sender:TObject; data:Integer);
var blk:TBlk;
begin
 Blky.GetBlkByID(data, blk);
 if ((blk = nil) or (blk.GetGlobalSettings.typ <> _BLK_ZAMEK)) then Exit();

 TBlkZamek(Blk).Zaver := false;
end;

procedure TChangeEventCaller.NullPrejezdZaver(Sender:TObject; data:Integer);
var blk:TBlk;
begin
 Blky.GetBlkByID(data, blk);
 if ((blk = nil) or (blk.GetGlobalSettings.typ <> _BLK_PREJEZD)) then Exit();

 TBlkPrejezd(Blk).Zaver := false;
end;

procedure TChangeEventCaller.NullTratZaver(Sender:TObject; data:Integer);
var blk:TBlk;
begin
 Blky.GetBlkByID(data, blk);
 if ((blk = nil) or (blk.GetGlobalSettings.typ <> _BLK_TRAT)) then Exit();

 TBlkTrat(Blk).Zaver := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TChangeEventCaller.NullVyhybkaMenuReduction(Sender:TObject; data:Integer);
var blk:TBlk;
begin
 Blky.GetBlkByID(data, blk);
 if ((blk = nil) or (blk.GetGlobalSettings.typ <> _BLK_VYH)) then Exit();

 TBlkVyhybka(Blk).ZrusRedukciMenu();
end;

procedure TChangeEventCaller.NullSComMenuReduction(Sender:TObject; data:Integer);
var blk:TBlk;
begin
 Blky.GetBlkByID(data, blk);
 if ((blk = nil) or (blk.GetGlobalSettings.typ <> _BLK_SCOM)) then Exit();

 TBlkSCom(Blk).ZrusRedukciMenu();
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  ceCaller := TChangeEventCaller.Create();
finalization
  ceCaller.Free();

end.
