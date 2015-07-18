unit TBlok;

//unita, ktera definuje technologicky blok jako abstraktni tridu

interface

uses IniFiles, TechnologieMTB, SysUtils, RPConst, TOblsRizeni,
      Generics.Collections, IdContext;

const
 //typy bloku
 _BLK_VYH     = 0;            // vyhybka
 _BLK_USEK    = 1;            // usek
 _BLK_IR      = 2;            // IR cidlo
 _BLK_SCOM    = 3;            // navestidlo
 _BLK_PREJEZD = 4;            // prejezd
 _BLK_TRAT    = 5;            // trat
 _BLK_UVAZKA  = 6;            // uvazka
 _BLK_ZAMEK   = 7;            // vyhybkovy zamek
 _BLK_ROZP    = 8;            // rozpojovac
 _BLK_TU      = 9;            // tratovy usek

 //if the block is disbled, this is is its state
 _BLK_DISABLED = -5;

 _BLK_MTBCNT = 4;
 _MAX_REDUCTION = 16;
 _MAX_EVENTS    = 16;

type
 //spolecne recordy:
 TMTBAddrs = record
  data:array[0.._BLK_MTBCNT-1] of TMTBAddr;
  Count:Byte;
 end;

 // pokud ja redukuji, zde si uchovavam, koho redukuji
 TReduction = array[0.._MAX_REDUCTION-1] of Integer;

 ///////////////////////////////
 TBlkSettings = record
  name:string;
  id:Integer;
  typ:Byte;
  poznamka:string;
 end;

 TOnBlkChange = procedure(Sender:TObject) of object;

 ///////////////////////////////

 // databaze funkci k zavolani pri urcite udalosti (napr. pri obsazeni useku se vola uzavreni prejezdu do jizdni cesty)

 TChangeEventFunc = procedure(Sender:TObject; data:Integer) of object;

 TChangeEvent = record
  func:TChangeEventFunc;
  data:Integer;
 end;

 TChangeEvents = TList<TChangeEvent>;

 TBlk = class(TObject)
  private
   changed:boolean;

  protected
   GlobalSettings:TBlkSettings;
   FOnChange:TOnBlkChange;  // childs can call the event
   ftable_index:Integer;
   ffrozen:boolean;
   ORsRef:TORsRef;          // ve kterych OR se blok nachazi

   //loading and saving MTB
   class function LoadMTB(ini:TMemIniFile;section:string):TMTBAddrs;
   class procedure SaveMTB(ini:TMemIniFile;section:string;data:TMTBAddrs);

   class procedure InitReduction(var db:TReduction);
   class procedure AddReduction(var db:TReduction; const blk_id:Integer);       // zredukovat a zaznemanat do DB
   class procedure RemoveReduction(var db:TReduction; const blk_id:Integer);    // odredukovat a smazat záznam z DB
   class procedure RemoveAllReduction(var db:TReduction);                       // odredukovat vsechny bloky
   class function IsReduction(var db:TReduction; const blk_id:Integer):boolean;     // Redukuji tento blok?

   class procedure PushMTBToOR(ORs:TORsRef; MTBs:TMTBAddrs);

   procedure CallChangeEvents(var events:TChangeEvents);

  public

   constructor Create(index:Integer);
   destructor Destroy(); override;

   procedure SetGlobalSettings(data:TBlkSettings);
   function GetGlobalSettings():TBlkSettings;

   procedure LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile); virtual;
   procedure SaveData(ini_tech:TMemIniFile;const section:string); virtual;
   procedure SaveStatus(ini_stat:TMemIniFile;const section:string); virtual; abstract;

   //enable or disable symbol on relief
   procedure Enable(); virtual; abstract;
   procedure Disable(); virtual;
   procedure Reset(); virtual;

   //update states
   procedure Update(); virtual;

   procedure Change(now:boolean = false); virtual; //will call the change event

   procedure Freeze(); virtual;
   procedure UnFreeze(); virtual;

   // zobrazuje menu, vraci string urcujici menu
   // kazdy blok ma sve zakladni menu, ktere obsahuje pouze hlavicku s jeho nazvem a oddelovac
   function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; virtual;

   // panel click je virtualni metoda, ktera v zakladu prazdna
   // u bloku, kde je zadouci osetrovat kliknuti na panel, je doporuceno ji pretizit,
   //   jinak je doporuceno ji vubec neimplementovat
   procedure PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights); virtual;

   procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string); virtual;

   class procedure AddChangeEvent(var events:TChangeEvents; func:TChangeEvent);
   class procedure RemoveChangeEvent(var events:TChangeEvents; func:TChangeEvent);
   class function CreateCHangeEvent(func:TChangeEventFunc; data:Integer = 0):TChangeEvent;

   //if some local variable is changed, this event is called to the program
   property OnChange:TOnBlkChange read FOnChange write FOnChange;
   property table_index:Integer read ftable_index write ftable_index;
   property frozen:boolean read ffrozen;
   property OblsRizeni:TORsRef read ORsRef;
 end;

implementation

uses TBloky, TBlokVyhybka, TBlokPrejezd, TBlokSCom, TBlokTrat, TBlokUsek,
      DataBloky, TBlokZamek;

////////////////////////////////////////////////////////////////////////////////

constructor TBlk.Create(index:Integer);
begin
 inherited Create();
 Self.ftable_index := index;
 Self.ffrozen      := false;
 Self.ORsRef.Cnt   := 0;
end;//ctor

destructor TBlk.Destroy();
begin
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlk.SetGlobalSettings(data:TBlkSettings);
begin
 Self.GlobalSettings := data;
end;//procedure

function TBlk.GetGlobalSettings():TBlkSettings;
begin
 Result := Self.GlobalSettings;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TBlk.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
begin
 Self.GlobalSettings.name     := ini_tech.ReadString(section, 'nazev', '');
 Self.GlobalSettings.id       := ini_tech.ReadInteger(section, 'id', 0);
 Self.GlobalSettings.typ      := ini_tech.ReadInteger(section, 'typ', -1);
 Self.GlobalSettings.poznamka := ini_tech.ReadString(section, 'pozn', '');
end;//procedure

procedure TBlk.SaveData(ini_tech:TMemIniFile;const section:string);
begin
 ini_tech.WriteString(section, 'nazev', Self.GlobalSettings.name);
 ini_tech.WriteInteger(section, 'id', Self.GlobalSettings.id);
 ini_tech.WriteInteger(section, 'typ', Self.GlobalSettings.typ);
 ini_tech.WriteString(section, 'pozn', Self.GlobalSettings.poznamka);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// mj. tady se zjistuje, ktera MTB jsou na kolejisti potreba
class function TBlk.LoadMTB(ini:TMemIniFile;section:string):TMTBAddrs;
var i:Integer;
begin
 Result.Count := ini.ReadInteger(section,'MTBcnt',0);
 for i := 0 to Result.Count-1 do
  begin
   Result.data[i].board := ini.ReadInteger(section,'MTBb'+IntToStr(i),0);
   Result.data[i].port  := ini.ReadInteger(section,'MTBp'+IntToStr(i),0);
   MTB.SetNeeded(Result.data[i].board);
  end;//for i
 for i := Result.Count to _BLK_MTBCNT-1 do
  begin
   Result.data[i].board := 0;
   Result.data[i].port  := 0;
  end;
end;//function

class procedure TBlk.SaveMTB(ini:TMemIniFile;section:string;data:TMTBAddrs);
var i:Integer;
begin
 ini.WriteInteger(section,'MTBcnt',data.Count);
 for i := 0 to data.Count-1 do
  begin
   ini.WriteInteger(section,'MTBb'+IntToStr(i),data.data[i].board);
   ini.WriteInteger(section,'MTBp'+IntToStr(i),data.data[i].port);
  end;//for i
end;//function

procedure TBlk.Change(now:boolean = false);
begin
 if (now) then
  begin
   Self.changed := false;
   if (Assigned(Self.FOnChange)) then Self.FOnChange(Self);
  end else
   Self.changed := true;

 BlokyTableData.BlkChange(Self.table_index);
end;//procedure

procedure TBlk.Freeze();
begin
 Self.ffrozen := true;
end;//procedure

procedure TBlk.UnFreeze();
begin
 Self.ffrozen := false;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// redukcni prikazy

class procedure TBlk.InitReduction(var db:TReduction);                                      // všude dát -1
var i:Integer;
begin
 for i := 0 to _MAX_REDUCTION-1 do
  db[i] := -1;
end;//procedure

class procedure TBlk.AddReduction(var db:TReduction; const blk_id:Integer);       // zredukovat a zaznemanat do DB
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to _MAX_REDUCTION-1 do
  if (db[i] < 0) then
   begin
    // pridat prvek do db a zredukovat

    // pozor: poradi je dulezite !! nejdriv pridat prvek do DB a pak redukovat (projevuje se pri staveni JC se spojkou)

    Blky.GetBlkByID(blk_id, Blk);
    if (Blk = nil) then Exit();

    db[i] := blk_id;

    case (Blk.GetGlobalSettings().typ) of
     _BLK_VYH     : (Blk as TBlkVyhybka).RedukujMenu();
     _BLK_PREJEZD : (Blk as TBlkPrejezd).Zaver := true;
     _BLK_SCOM    : (Blk as TBlkSCom).RedukujMenu();
    end;

    Exit();
   end;
end;//procedure

// pokud blk_id = -1, tak odredukuji vsechno
class procedure TBlk.RemoveReduction(var db:TReduction; const blk_id:Integer);    // odredukovat a smazat záznam z DB
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to _MAX_REDUCTION-1 do
  if (((db[i] = blk_id) and (blk_id > -1)) or ((blk_id = -1) and (db[i] > -1))) then
   begin
    // smazat prvek z db a zrusit redukci

    // POZOR: poradi je dulezite; njeprve smazat z DB, pak zrusit redukci

    Blky.GetBlkByID(db[i], Blk);
    if (Blk = nil) then Exit();

    db[i] := -1;

    case (Blk.GetGlobalSettings().typ) of
     _BLK_VYH     : (Blk as TBlkVyhybka).ZrusRedukciMenu();
     _BLK_PREJEZD : (Blk as TBlkPrejezd).Zaver := false;
     _BLK_SCOM    : (Blk as TBlkSCom).ZrusRedukciMenu();
     _BLK_TRAT    : (Blk as TBlkTrat).Zaver := false;
     _BLK_USEK    : (Blk as TBlkUsek).Zaver := TJCType.no;
     _BLK_ZAMEK   : (Blk as TBlkZamek).Zaver := false;
    end;

   end;
end;//procedure

class procedure TBlk.RemoveAllReduction(var db:TReduction);                           // odredukovat vsechny bloky, ktere mam ulozene
begin
 Self.RemoveReduction(db, -1);
end;//procedure

class function TBlk.IsReduction(var db:TReduction; const blk_id:Integer):boolean;     // Redukuji tento blok?
var i:Integer;
begin
 for i := 0 to _MAX_REDUCTION-1 do
  if (db[i] = blk_id) then
   Exit(true);
 Exit(false);
end;//function

////////////////////////////////////////////////////////////////////////////////

class procedure TBlk.PushMTBToOR(ORs:TORsRef; MTBs:TMTbAddrs);
var i, j:Integer;
begin
 for i := 0 to ORs.Cnt-1 do
   for j := 0 to MTBs.Count-1 do
     ORs.ORs[i].MTBAdd(MTBs.data[j].board);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlk.CallChangeEvents(var events:TChangeEvents);
var i:Integer;
begin
 for i := 0 to events.Count-1 do
  if (Assigned(events[i].func)) then
    events[i].func(Self, events[i].data);
 events.Clear();
end;//procedure

class procedure TBlk.AddChangeEvent(var events:TChangeEvents; func:TChangeEvent);
begin
 events.Add(func);
end;//procedure

class procedure TBlk.RemoveChangeEvent(var events:TChangeEvents; func:TChangeEvent);
var i:Integer;
begin
 for i := 0 to events.Count-1 do
   if ((@events[i].func = @func.func) and (events[i].data = func.data)) then
    begin
     events.Delete(i);
     Exit();
    end;
end;//procedure

class function TBlk.CreateCHangeEvent(func:TChangeEventFunc; data:Integer = 0):TChangeEvent;
begin
 Result.func := func;
 Result.data := data;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TBlk.Disable();
begin
 Self.ffrozen := false;
end;//procedure

procedure TBlk.Reset();
begin

end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlk.Update();
begin
 if (Self.changed) then
  begin
   if (Assigned(Self.FOnChange)) then Self.FOnChange(Self);
   Self.changed := false;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// zobrazuje menu, vraci string urcujici menu
// kazdy blok ma sve zakladni menu, ktere obsahuje pouze hlavicku s jeho nazvem a oddelovac
function TBlk.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
begin
 Result := '$'+Self.GlobalSettings.name+',-,';
end;//function

procedure TBlk.PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights);
begin

end;

procedure TBlk.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string);
begin
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit

