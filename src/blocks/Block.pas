﻿unit Block;

{ TBlk abstract class definition.
  TBlk is abstract class for rechnological blocks. }

interface

uses IniFiles, TechnologieRCS, SysUtils, AreaDb, Generics.Collections,
  IdContext, JsonDataObjects, Area, changeEvent, Classes;

const
  // if the block is disbled, this is is its state
  _BLK_DISABLED = -5;

  _BLK_RCS_CNT = 4;
  _MAX_EVENTS = 16;

type
  ETypeNotFound = class(Exception);

  TBlkType = (btAny = -1, btTurnout = 0, btTrack = 1, btIr = 2, btSignal = 3, btCrossing = 4, btRailway = 5,
    btLinker = 6, btLock = 7, btDisconnector = 8, btRT = 9, btIO = 10, btSummary = 11, btAC = 12, btGroupSignal = 13);

  TZaver = (undefinned = -1, no = 0, vlak = 1, posun = 2, nouz = 3, staveni = 4, ab = 5);
  TRCSAddrs = TList<TRCSAddr>;

  /// ////////////////////////////
  TBlkSettings = record
    name: string;
    id: Integer;
    typ: TBlkType;
    note: string;
  end;

  TOnBlkChange = procedure(Sender: TObject) of object;

  /// ////////////////////////////

  TBlk = class(TObject)
  private const
    _def_glob_settings: TBlkSettings = (name: ''; id: - 1; note: '';);
  private
    changed: Boolean;

  protected
    m_globSettings: TBlkSettings;
    m_areas: TList<TArea>; // ve kterych OR se blok nachazi
    FOnChange: TOnBlkChange; // childs can call the event
    ftable_index: Integer;
    ffrozen: Boolean;

    class function LoadRCS(ini: TMemIniFile; section: string): TRCSAddrs;
    class procedure SaveRCS(ini: TMemIniFile; section: string; data: TRCSAddrs);

    class procedure PushRCSToArea(areas: TList<TArea>; RCSs: TRCSAddrs); overload;
    class procedure PushRCSToArea(areas: TList<TArea>; RCS: TRCSAddr); overload;

    procedure CallChangeEvents(var events: TChangeEvents);
    function LoadORs(ini: TMemIniFile; section: string): TStrings; // user must free result!

  public

    constructor Create(index: Integer);
    destructor Destroy(); override;

    procedure SetGlobalSettings(data: TBlkSettings); virtual;
    function GetGlobalSettings(): TBlkSettings; virtual;

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); virtual;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); virtual;
    procedure SaveStatus(ini_stat: TMemIniFile; const section: string); virtual;

    procedure Enable(); virtual; abstract;
    procedure Disable(); virtual;
    procedure Reset(); virtual;
    procedure AfterLoad(); virtual;
    // AfterLoad je volano po nacteni vsech dat, slouzi napriklad pro vytvoreni tratFlagu

    procedure Update(); virtual;
    procedure Change(now: Boolean = false); virtual; // will call the change event

    procedure Freeze(); virtual;
    procedure UnFreeze(); virtual;

    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; virtual;

    // zobrazuje menu, vraci string urcujici menu
    // kazdy blok ma sve zakladni menu, ktere obsahuje pouze hlavicku s jeho nazvem a oddelovac
    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string; virtual;

    // panel click je virtualni metoda, ktera v zakladu prazdna
    // u bloku, kde je zadouci osetrovat kliknuti na panel, je doporuceno ji pretizit,
    // jinak je doporuceno ji vubec neimplementovat
    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
      params: string = ''); virtual;
    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); virtual;
    function PanelStateString(): string; virtual;

    // Tyto procedury vraci json objekt do \json, z dedicich bloku
    // je nutno volat inherited.
    procedure GetPtData(json: TJsonObject; includeState: Boolean); virtual;
    procedure GetPtState(json: TJsonObject); virtual;
    procedure PutPtState(reqJson: TJsonObject; respJson: TJsonObject); virtual;

    function IsInArea(Area: TObject): Boolean;

    class procedure AddChangeEvent(var events: TChangeEvents; func: TChangeEvent);
    class procedure RemoveChangeEvent(var events: TChangeEvents; func: TChangeEvent);

    class function BlkTypeToStr(typ: TBlkType): string;
    class function BlkTypeFromStr(typ: string): TBlkType;
    class procedure RCSstoJSON(const addrs: TRCSAddrs; json: TJsonArray);
    class procedure RCStoJSON(const addr: TRCSAddr; json: TJsonObject);

    // if some local variable is changed, this event is called to the program
    property OnChange: TOnBlkChange read FOnChange write FOnChange;
    property table_index: Integer read ftable_index write ftable_index;
    property frozen: Boolean read ffrozen;
    property areas: TList<TArea> read m_areas;

    property id: Integer read m_globSettings.id;
    property name: string read m_globSettings.name;
    property typ: TBlkType read m_globSettings.typ;
    property note: string read m_globSettings.note;
  end;

implementation

uses BlockDb, DataBloky, appEv, ownStrUtils, Diagnostics;

/// /////////////////////////////////////////////////////////////////////////////

constructor TBlk.Create(index: Integer);
begin
  inherited Create();
  Self.m_globSettings := _def_glob_settings;
  Self.ftable_index := index;
  Self.ffrozen := false;
  Self.m_areas := TList<TArea>.Create();
end;

destructor TBlk.Destroy();
begin
  Self.m_areas.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlk.SetGlobalSettings(data: TBlkSettings);
var id_changed: Boolean;
begin
  id_changed := ((Self.id <> data.id) and (Self.id <> -1));
  Self.m_globSettings := data;
  if (id_changed) then
  begin
    // sem se skoci, pokud je potreba preskladat bloky, protoze doslo ke zmene ID
    // pri vytvareni novych bloku se sem neskace
    Blocks.BlkIDChanged(Self.table_index);
  end;
end;

function TBlk.GetGlobalSettings(): TBlkSettings;
begin
  Result := Self.m_globSettings;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlk.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
begin
  Self.m_globSettings.name := ini_tech.ReadString(section, 'nazev', '');
  Self.m_globSettings.id := StrToInt(section);
  Self.m_globSettings.typ := TBlkType(ini_tech.ReadInteger(section, 'typ', -1));
  Self.m_globSettings.note := ini_tech.ReadString(section, 'pozn', '');
end;

procedure TBlk.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  ini_tech.WriteString(section, 'nazev', Self.m_globSettings.name);
  ini_tech.WriteInteger(section, 'typ', Integer(Self.m_globSettings.typ));

  if (Self.m_globSettings.note <> '') then
    ini_tech.WriteString(section, 'pozn', Self.m_globSettings.note);
end;

procedure TBlk.SaveStatus(ini_stat: TMemIniFile; const section: string);
begin

end;

/// /////////////////////////////////////////////////////////////////////////////

// mj. tady se zjistuje, ktere moduly RCS jsou na kolejisti potreba
class function TBlk.LoadRCS(ini: TMemIniFile; section: string): TRCSAddrs;
var count: Integer;
  prefix: string;
begin
  Result := TList<TechnologieRCS.TRCSAddr>.Create(RCSAddrComparer);

  prefix := 'RCS';
  count := ini.ReadInteger(section, prefix + 'cnt', 0);
  if (count = 0) then // backward compatibility
  begin
    prefix := 'MTB';
    count := ini.ReadInteger(section, prefix + 'cnt', 0);
  end;

  for var i: Integer := 0 to count - 1 do
  begin
    var rcsAddr: TRCSAddr := TRCS.rcsAddr(ini.ReadInteger(section, prefix + 'b' + IntToStr(i), 0),
      ini.ReadInteger(section, prefix + 'p' + IntToStr(i), 0));
    Result.Add(rcsAddr);
    if ((rcsAddr.board > 0) or (rcsAddr.port > 0)) then
      RCSi.SetNeeded(rcsAddr.board);
  end;
end;

class procedure TBlk.SaveRCS(ini: TMemIniFile; section: string; data: TRCSAddrs);
begin
  if (data.count > 0) then
    ini.WriteInteger(section, 'RCScnt', data.count);

  for var i: Integer := 0 to data.count - 1 do
  begin
    if ((data[i].board > 0) or (data[i].port > 0)) then
    begin
      ini.WriteInteger(section, 'RCSb' + IntToStr(i), data[i].board);
      ini.WriteInteger(section, 'RCSp' + IntToStr(i), data[i].port);
    end;
  end;
end;

procedure TBlk.Change(now: Boolean = false);
begin
  if (now) then
  begin
    Self.changed := false;
    if (Assigned(Self.FOnChange)) then
      Self.FOnChange(Self);
  end
  else
    Self.changed := true;

  BlokyTableData.BlkChange(Self.table_index);
end;

procedure TBlk.Freeze();
begin
  Self.ffrozen := true;
end;

procedure TBlk.UnFreeze();
begin
  Self.ffrozen := false;
end;

function TBlk.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

class procedure TBlk.PushRCSToArea(areas: TList<TArea>; RCSs: TRCSAddrs);
begin
  for var area: TArea in areas do
    for var rcsAddr: TRCSAddr in RCSs do
      area.RCSAdd(rcsAddr.board);
end;

class procedure TBlk.PushRCSToArea(areas: TList<TArea>; RCS: TRCSAddr);
begin
  for var area: TArea in areas do
    Area.RCSAdd(RCS.board);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlk.CallChangeEvents(var events: TChangeEvents);
begin
  for var i: Integer := 0 to events.count - 1 do
    if (Assigned(events[i].func)) then
    begin
      try
        events[i].func(Self, events[i].data);
      except
        on E: Exception do
          AppEvents.LogException(E, 'CallChengeEvents exception : ' + E.Message);
      end;
    end;
  events.Clear();
end;

class procedure TBlk.AddChangeEvent(var events: TChangeEvents; func: TChangeEvent);
begin
  events.Add(func);
end;

class procedure TBlk.RemoveChangeEvent(var events: TChangeEvents; func: TChangeEvent);
begin
  for var i: Integer := events.count - 1 downto 0 do
    if (events[i] = func) then
      events.Delete(i);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlk.Disable();
begin
  Self.ffrozen := false;
end;

procedure TBlk.Reset();
begin

end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlk.Update();
begin
  if (Self.changed) then
  begin
    if (Assigned(Self.FOnChange)) then
      Self.FOnChange(Self);
    Self.changed := false;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// zobrazuje menu, vraci string urcujici menu
// kazdy blok ma sve zakladni menu, ktere obsahuje pouze hlavicku s jeho nazvem a oddelovac
function TBlk.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
begin
  Result := '$' + Self.name + ',';
  if (diag.showBlockId) then
    Result := Result + '$' + IntToStr(Self.id) + ',';
  Result := Result + '-,'
end;

procedure TBlk.PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
  params: string = '');
begin
  // This function should be empty.
end;

procedure TBlk.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
begin
  // This function should be empty.
end;

function TBlk.PanelStateString(): string;
begin
  Result := IntToStr(Integer(Self.typ)) + ';' + IntToStr(Self.id) + ';';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlk.AfterLoad();
begin
  // This function should be empty.
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlk.GetPtData(json: TJsonObject; includeState: Boolean);
begin
  json['name'] := Self.name;
  json['id'] := Self.id;
  json['type'] := TBlk.BlkTypeToStr(Self.typ);
  for var area: TArea in Self.areas do
    json.A['areas'].Add(area.id);
end;

procedure TBlk.GetPtState(json: TJsonObject);
begin
  // This function should be empty.
end;

procedure TBlk.PutPtState(reqJson: TJsonObject; respJson: TJsonObject);
begin
  Self.GetPtState(respJson.O['blockState']);
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TBlk.BlkTypeToStr(typ: TBlkType): string;
begin
  case (typ) of
    btTurnout:
      Result := 'turnout';
    btTrack:
      Result := 'track';
    btIr:
      Result := 'ir';
    btSignal:
      Result := 'signal';
    btCrossing:
      Result := 'crossing';
    btRailway:
      Result := 'railway';
    btLinker:
      Result := 'linker';
    btLock:
      Result := 'lock';
    btDisconnector:
      Result := 'disconnector';
    btRT:
      Result := 'railwayTrack';
    btIO:
      Result := 'io';
    btSummary:
      Result := 'crossingsSummary';
    btAC:
      Result := 'AC';
    btGroupSignal:
      Result := 'groupSignal';
  else
    Result := 'neznamy';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

class procedure TBlk.RCSstoJSON(const addrs: TRCSAddrs; json: TJsonArray);
begin
  for var rcsAddr: TRCSAddr in addrs do
  begin
    var newObj: TJsonObject := json.AddObject();
    newObj['board'] := rcsAddr.board;
    newObj['port'] := rcsAddr.port;
  end;
end;

class procedure TBlk.RCStoJSON(const addr: TRCSAddr; json: TJsonObject);
begin
  json['board'] := addr.board;
  json['port'] := addr.port;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlk.IsInArea(Area: TObject): Boolean;
begin
  Result := Self.areas.Contains(Area as TArea);
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TBlk.BlkTypeFromStr(typ: string): TBlkType;
begin
  if (typ = 'vyhybka') or (typ = 'výhybka') or (typ = 'turnout') then
    Result := btTurnout
  else if (typ = 'usek') or (typ = 'úsek') or (typ = 'track') then
    Result := btTrack
  else if (typ = 'ir') then
    Result := btIr
  else if (typ = 'navestidlo') or (typ = 'signal') then
    Result := btSignal
  else if (typ = 'prejezd') or (typ = 'přejezd') or (typ = 'crossing') then
    Result := btCrossing
  else if (typ = 'trat') or (typ = 'trať') or (typ = 'railway') then
    Result := btRailway
  else if (typ = 'uvazka') or (typ = 'úvazka') or (typ = 'linker') then
    Result := btLinker
  else if (typ = 'rozp') or (typ = 'rozpojovac') or (typ = 'rozpojovač') or (typ = 'disconnector') then
    Result := btDisconnector
  else if ((typ = 'tratUsek') or (typ = 'traťÚsek') or (typ = 'tu') or (typ = 'TU') or (typ = 'railwayTrack') or
    (typ = 'rt')) then
    Result := btRT
  else if (typ = 'io') then
    Result := btIO
  else
    raise ETypeNotFound.Create('Blok typu ' + typ + ' neexistuje');
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlk.LoadORs(ini: TMemIniFile; section: string): TStrings;
begin
  if (ini = nil) then
  begin
    Self.m_areas.Clear();
    Exit(TStringList.Create());
  end;

  var strs: TStrings := TStringList.Create();
  try
    ExtractStringsEx([';'], [], ini.ReadString(section, IntToStr(Self.id), ''), strs);
    if (strs.count < 1) then
      Exit(strs);
    if (Self.m_areas <> nil) then
      Self.m_areas.Free();
    Self.m_areas := AreaDb.areas.ParseORs(strs[0]);
  except
    strs.Free();
    raise;
  end;
  Result := strs;
end;
/// /////////////////////////////////////////////////////////////////////////////

end.// unit
