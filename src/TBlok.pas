unit TBlok;

//unita, ktera definuje technologicky blok jako abstraktni tridu

interface

uses IniFiles, TechnologieRCS, SysUtils, TOblsRizeni,
      Generics.Collections, IdContext, JsonDataObjects, TOblRizeni, changeEvent;

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
 _BLK_VYSTUP  = 10;           // logicky vystup

 //if the block is disbled, this is is its state
 _BLK_DISABLED = -5;

 _BLK_MTBCNT = 4;
 _MAX_EVENTS    = 16;

type
 ETypeNotFound = class(Exception);

 TZaver = (undefinned = -1, no = 0, vlak = 1, posun = 2, nouz = 3, staveni = 4);

 //spolecne recordy:
 TRCSAddrs = record
  data:array[0.._BLK_MTBCNT-1] of TRCSAddr;
  Count:Byte;
 end;

 ///////////////////////////////
 TBlkSettings = record
  name:string;
  id:Integer;
  typ:Byte;
  poznamka:string;
 end;

 TOnBlkChange = procedure(Sender:TObject) of object;

 ///////////////////////////////

 TBlk = class(TObject)
  private const
    _def_glob_settings:TBlkSettings = (
      name: '';
      id: -1;
      typ: 0;
      poznamka: '';
    );
  private
   changed:boolean;

  protected
   GlobalSettings:TBlkSettings;
   FOnChange:TOnBlkChange;  // childs can call the event
   ftable_index:Integer;
   ffrozen:boolean;
   ORsRef:TORsRef;          // ve kterych OR se blok nachazi

   //loading and saving MTB
   class function LoadRCS(ini:TMemIniFile;section:string):TRCSAddrs;
   class procedure SaveRCS(ini:TMemIniFile;section:string;data:TRCSAddrs);

   class procedure PushRCSToOR(ORs:TORsRef; RCSs:TRCSAddrs);

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
   procedure AfterLoad(); virtual;    // AfterLoad je volano po nacteni vsech dat, slouzi napriklad pro vytvoreni tratFlagu

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

   procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer); virtual;

   // Tyto procedury vraci json objekt do \json, z dedicich bloku
   // je nutno volat inherited.
   procedure GetPtData(json:TJsonObject; includeState:boolean); virtual;
   procedure GetPtState(json:TJsonObject); virtual;
   procedure PostPtState(reqJson:TJsonObject; respJson:TJsonObject); virtual;

   function IsInOR(OblR:TObject):boolean;

   class procedure AddChangeEvent(var events:TChangeEvents; func:TChangeEvent);
   class procedure RemoveChangeEvent(var events:TChangeEvents; func:TChangeEvent);
   class function CreateChangeEvent(func:TChangeEventFunc; data:Integer = 0):TChangeEvent;

   class function BlkTypeToStr(typ:Byte):string;
   class function BlkTypeFromStr(typ:string):Byte;
   class procedure RCSstoJSON(const addrs:TRCSAddrs; json:TJsonArray);
   class procedure RCStoJSON(const addr:TRCSAddr; json:TJsonObject);

   //if some local variable is changed, this event is called to the program
   property OnChange:TOnBlkChange read FOnChange write FOnChange;
   property table_index:Integer read ftable_index write ftable_index;
   property frozen:boolean read ffrozen;
   property OblsRizeni:TORsRef read ORsRef;
 end;

implementation

uses TBloky, TBlokVyhybka, TBlokPrejezd, TBlokSCom, TBlokTrat, TBlokUsek,
      DataBloky, TBlokZamek, appEv;

////////////////////////////////////////////////////////////////////////////////

constructor TBlk.Create(index:Integer);
begin
 inherited Create();
 Self.GlobalSettings := _def_glob_settings;
 Self.ftable_index   := index;
 Self.ffrozen        := false;
 Self.ORsRef.Cnt     := 0;
end;//ctor

destructor TBlk.Destroy();
begin
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlk.SetGlobalSettings(data:TBlkSettings);
var id_changed:boolean;
begin
 id_changed := ((Self.GlobalSettings.id <> data.id) and (Self.GlobalSettings.id <> -1));
 Self.GlobalSettings := data;
 if (id_Changed) then
  begin
   // sem se skoci, pokud je potreba preskladat bloky, protoze doslo ke zmene ID
   // pri vytvareni novych bloku se sem neskace
   Blky.BlkIDChanged(Self.table_index);
  end;
end;//procedure

function TBlk.GetGlobalSettings():TBlkSettings;
begin
 Result := Self.GlobalSettings;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TBlk.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
begin
 Self.GlobalSettings.name     := ini_tech.ReadString(section, 'nazev', '');
 Self.GlobalSettings.id       := StrToInt(section);
 Self.GlobalSettings.typ      := ini_tech.ReadInteger(section, 'typ', -1);
 Self.GlobalSettings.poznamka := ini_tech.ReadString(section, 'pozn', '');
end;//procedure

procedure TBlk.SaveData(ini_tech:TMemIniFile;const section:string);
begin
 ini_tech.WriteString(section, 'nazev', Self.GlobalSettings.name);
 ini_tech.WriteInteger(section, 'typ', Self.GlobalSettings.typ);

 if (Self.GlobalSettings.poznamka <> '') then
   ini_tech.WriteString(section, 'pozn', Self.GlobalSettings.poznamka);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// mj. tady se zjistuje, ktere moduly RCS jsou na kolejisti potreba
class function TBlk.LoadRCS(ini:TMemIniFile;section:string):TRCSAddrs;
var i:Integer;
begin
 Result.Count := ini.ReadInteger(section,'MTBcnt',0);
 for i := 0 to Result.Count-1 do
  begin
   Result.data[i].board := ini.ReadInteger(section,'MTBb'+IntToStr(i),0);
   Result.data[i].port  := ini.ReadInteger(section,'MTBp'+IntToStr(i),0);
   RCSi.SetNeeded(Result.data[i].board);
  end;//for i
 for i := Result.Count to _BLK_MTBCNT-1 do
  begin
   Result.data[i].board := 0;
   Result.data[i].port  := 0;
  end;
end;//function

class procedure TBlk.SaveRCS(ini:TMemIniFile;section:string;data:TRCSAddrs);
var i:Integer;
begin
 if (data.Count > 0) then
   ini.WriteInteger(section, 'MTBcnt', data.Count);

 for i := 0 to data.Count-1 do
  begin
   ini.WriteInteger(section, 'MTBb'+IntToStr(i), data.data[i].board);
   ini.WriteInteger(section, 'MTBp'+IntToStr(i), data.data[i].port);
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

class procedure TBlk.PushRCSToOR(ORs:TORsRef; RCSs:TRCSAddrs);
var i, j:Integer;
begin
 for i := 0 to ORs.Cnt-1 do
   for j := 0 to RCSs.Count-1 do
     ORs.ORs[i].MTBAdd(RCSs.data[j].board);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlk.CallChangeEvents(var events:TChangeEvents);
var i:Integer;
begin
 for i := 0 to events.Count-1 do
  if (Assigned(events[i].func)) then
   begin
    try
      events[i].func(Self, events[i].data);
    except
      on E:Exception do
         AppEvents.LogException(E, 'CallChengeEvents exception : '+E.Message);
    end;
   end;
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
 // This function should be empty.
end;

procedure TBlk.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer);
begin
 // This function should be empty.
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlk.AfterLoad();
begin
 // This function should be empty.
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlk.GetPtData(json:TJsonObject; includeState:boolean);
begin
 json['nazev'] := Self.GlobalSettings.name;
 json['id']    := Self.GlobalSettings.id;
 json['typ']   := TBlk.BlkTypeToStr(Self.GlobalSettings.typ);
end;

procedure TBlk.GetPtState(json:TJsonObject);
begin
 // This function should be empty.
end;

procedure TBlk.PostPtState(reqJson:TJsonObject; respJson:TJsonObject);
begin
 Self.GetPtState(respJson.O['blokStav']);
end;

////////////////////////////////////////////////////////////////////////////////

class function TBlk.BlkTypeToStr(typ:Byte):string;
begin
 case (typ) of
  _BLK_VYH     : Result := 'vyhybka';
  _BLK_USEK    : Result := 'usek';
  _BLK_IR      : Result := 'ir';
  _BLK_SCOM    : Result := 'scom';
  _BLK_PREJEZD : Result := 'prejezd';
  _BLK_TRAT    : Result := 'trat';
  _BLK_UVAZKA  : Result := 'uvazka';
  _BLK_ZAMEK   : Result := 'zamek';
  _BLK_ROZP    : Result := 'rozpojovac';
  _BLK_TU      : Result := 'tratUsek';
  _BLK_VYSTUP  : Result := 'vystup';
 else
  Result := 'neznamy';
 end;
end;

////////////////////////////////////////////////////////////////////////////////

class procedure TBlk.RCSstoJSON(const addrs:TRCSAddrs; json:TJsonArray);
var i:Integer;
    newObj:TJsonObject;
begin
 for i := 0 to addrs.Count-1 do
  begin
   newObj := json.AddObject();
   newObj['board'] := addrs.data[i].board;
   newObj['port'] := addrs.data[i].port;
  end;
end;

class procedure TBlk.RCStoJSON(const addr:TRCSAddr; json:TJsonObject);
begin
 json['board'] := addr.board;
 json['port'] := addr.port;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlk.IsInOR(OblR:TObject):boolean;
var i:Integer;
begin
 for i := 0 to Self.OblsRizeni.Cnt-1 do
   if (Self.OblsRizeni.ORs[i] = OblR) then
     Exit(true);
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

class function TBlk.BlkTypeFromStr(typ:string):Byte;
begin
 if (typ = 'vyhybka') or (typ = 'v˝hybka') then Result := _BLK_VYH
 else if (typ = 'usek') or (typ = '˙sek') then Result := _BLK_USEK
 else if (typ = 'ir') then Result := _BLK_IR
 else if (typ = 'scom') then Result := _BLK_SCOM
 else if (typ = 'prejezd') or (typ = 'p¯ejezd') then Result := _BLK_PREJEZD
 else if (typ = 'trat') or (typ = 'traù') then Result := _BLK_TRAT
 else if (typ = 'uvazka') or (typ = '˙vazka') then Result := _BLK_UVAZKA
 else if (typ = 'rozp') or (typ = 'rozpojovac') or (typ = 'rozpojovaË') then Result := _BLK_ROZP
 else if (typ = 'tratUsek') or (typ = 'traù⁄sek') or (typ = 'tu') or (typ = 'TU') then Result := _BLK_TU
 else if (typ = 'vystup') or (typ = 'v˝stup') then Result := _BLK_VYSTUP
 else raise ETypeNotFound.Create('Blok typu '+typ+' neexistuje');
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit

