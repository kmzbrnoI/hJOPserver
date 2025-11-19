unit AreaDb;

{ TAreas class is a database of areas (TArea). }

interface

uses Area, IniFiles, SysUtils, Classes, COmCtrls, IdContext,
  StdCtrls, Generics.Collections, JsonDataObjects, RCSsc;

type
  TAreas = class
  private const
    _SECT_AREAS = 'OR';

  private
    db: TObjectList<TArea>;
    fstat_filename: string;

    function GetAreasCnt(): Integer;

  public
    constructor Create();
    destructor Destroy(); override;

    procedure LoadData(const filename: string; const stat_filename: string);
    procedure SaveState(const filename: string);

    function Get(index: Integer): TArea; overload;
    function Get(id: string): TArea; overload;

    function ParseORs(str: string): TList<TArea>;
    procedure RCSFail(addr: TRCSsSystemModule); // je vyvolano pri vypadku RCS modulu, resi zobrazeni chyby do panelu v OR

    procedure Update();
    procedure DisconnectPanels();
    procedure SendORList(Context: TIdContext); // odesle seznam vsech OR na spojeni \Context

    procedure FillCB(CB: TComboBox; selected: TArea); // naplni ComboBox seznamem oblasti rizeni
    procedure InitLights();
    // broadcast chybove hlasky, ktera ma jit jen panelum,
    // kde alespon jeden je minimalne opravneni min_rights
    procedure BroadcastBottomError(err: string; tech: string; min_rights: TAreaRights = read);
    procedure BroadcastPlaySound(sound_code: Integer; loop: Boolean = false; min_rights: TAreaRights = read);

    function GetEnumerator(): TEnumerator<TArea>;

    procedure GetPtData(json: TJsonObject; dict: Boolean = false);

    property Items[index: Integer]: TArea read Get; default;
    property Count: Integer read GetAreasCnt;

    property status_filename: string read fstat_filename;
  end;

var
  Areas: TAreas;

implementation

uses Logging, TCPServerPanel, TRVDatabase, appEv, PTUtils, ownStrUtils, Config;

/// /////////////////////////////////////////////////////////////////////////////

constructor TAreas.Create();
begin
  inherited;
  Self.db := TObjectList<TArea>.Create(TArea.IdComparer());
end;

destructor TAreas.Destroy();
begin
  Self.db.Free();
  inherited Destroy();
end;

/// /////////////////////////////////////////////////////////////////////////////

// nacitani OR a vytvareni vsech OR
procedure TAreas.LoadData(const filename: string; const stat_filename: string);
var ini, ini_stat: TMemIniFile;
begin
  Log('Načítám stanice - ' + filename, llInfo, lsData);
  Self.fstat_filename := stat_filename;

  if (not FileExists(filename)) then
    raise EFileNotFound.Create('Soubor se stanicemi neexistuje - ' + filename);

  Self.db.Clear();
  ini := TMemIniFile.Create(filename, TEncoding.UTF8);
  ini_stat := TMemIniFile.Create(stat_filename, TEncoding.UTF8);
  var areas: TStrings := TStringList.Create();

  try
    ini.ReadSection(_SECT_AREAS, areas);

    for var i: Integer := 0 to areas.Count - 1 do
    begin
      var area: TArea := TArea.Create(i + 1);
      try
        area.LoadData(ini.ReadString(_SECT_AREAS, areas[i], ''));
        area.LoadStat(ini_stat, area.id);
        Self.db.Add(area);
      except
        on E: Exception do
        begin
          AppEvents.LogException(E, 'Nacitam oblast rizeni ' + IntToStr(i));
          area.Free();
        end;
      end;
    end;

    Self.db.Sort();
    for var i: Integer := 0 to Self.db.Count - 1 do
      Self.db[i].index := i;
  finally
    areas.Free();
    ini.Free();
    ini_stat.Free();
  end;

  Log('Načteno ' + IntToStr(Self.db.Count) + ' stanic', llInfo, lsData);
end;

/// /////////////////////////////////////////////////////////////////////////////
// ukladani stavu vsech oblasti rizeni

procedure TAreas.SaveState(const filename: string);
begin
  var ini: TMemIniFile := TMemIniFile.Create(filename, TEncoding.UTF8);

  for var area: TArea in Self.db do
  begin
    try
      Area.SaveStat(ini, Area.id);
    except
      on E: Exception do
        AppEvents.LogException(E, 'Ukladani stavu OR ' + Area.id);
    end;
  end;

  ini.UpdateFile();
  ini.Free();
end;

/// /////////////////////////////////////////////////////////////////////////////

// parsing OR stringu
function TAreas.ParseORs(str: string): TList<TArea>;
begin
  var parsed: TStrings := TStringList.Create();
  try
    ExtractStringsEx(['|', ','], [], str, parsed);

    Result := TList<TArea>.Create();
    for var areaid: string in parsed do
      Result.Add(Self.Get(areaid));
  finally
    parsed.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TAreas.Get(index: Integer): TArea;
begin
  Result := Self.db[index];
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TAreas.Update();
begin
  for var i: Integer := 0 to Self.db.Count - 1 do
    Self.db[i].Update();
end;

procedure TAreas.DisconnectPanels();
begin
  for var i: Integer := 0 to Self.db.Count - 1 do
    Self.db[i].DisconnectPanels();

  // vymazeme vsechny otevrene regulatory u klientu
  for var i: Integer := 0 to _MAX_ADDR - 1 do
    if (Assigned(RVDb[i])) then
      RVDb[i].state.regulators.Clear();
end;

procedure TAreas.SendORList(Context: TIdContext);
var str: string;
begin
  str := '-;OR-LIST;';
  for var i: Integer := 0 to Self.db.Count - 1 do
    str := str + '[' + Self.db[i].id + ',' + Self.db[i].Name + ']';

  PanelServer.SendLn(Context, str);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TAreas.RCSFail(addr: TRCSsSystemModule);
begin
  for var area: TArea in Self.db do
    area.RCSFail(addr);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TAreas.FillCB(CB: TComboBox; selected: TArea);
begin
  CB.Clear();
  for var i: Integer := 0 to Self.db.Count - 1 do
  begin
    CB.Items.Add(Self.db[i].Name);
    if (Self.db[i] = selected) then
      CB.ItemIndex := i;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TAreas.GetAreasCnt(): Integer;
begin
  Result := Self.db.Count;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TAreas.InitLights();
begin
  for var area: TArea in Self.db do
    area.InitLights();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TAreas.BroadcastBottomError(err: string; tech: string; min_rights: TAreaRights = read);
var clients: TDictionary<TIdContext, Boolean>; // set
begin
  clients := TDictionary<TIdContext, Boolean>.Create();
  for var area: TArea in Self.db do
    for var connected: TAreaPanel in area.connected do
      if (connected.Rights >= min_rights) then
        clients.AddOrSetValue(connected.Panel, true);

  for var client: TIdContext in clients.Keys do
    PanelServer.BottomError(client, err, '-', tech);

  clients.Free();
end;

procedure TAreas.BroadcastPlaySound(sound_code: Integer; loop: Boolean = false; min_rights: TAreaRights = read);
var clients: TDictionary<TIdContext, Boolean>; // set
begin
  clients := TDictionary<TIdContext, Boolean>.Create();
  for var area: TArea in Self.db do
    for var connected: TAreaPanel in Area.connected do
      if (connected.Rights >= min_rights) then
        clients.AddOrSetValue(connected.Panel, true);

  for var client: TIdContext in clients.Keys do
    PanelServer.PlaySound(client, sound_code, loop);

  clients.Free();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TAreas.Get(id: string): TArea;
var left, right, mid: Integer;
begin
  left := 0;
  right := Self.db.Count - 1;

  while (left <= right) do
  begin
    mid := (left + right) div 2;
    if (Self.db[mid].id = id) then
      Exit(Self.db[mid]);

    if (CompareStr(id, Self.db[mid].id, loUserLocale) < 0) then
      right := mid - 1
    else
      left := mid + 1;
  end;
  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TAreas.GetEnumerator(): TEnumerator<TArea>;
begin
  Result := Self.db.GetEnumerator();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TAreas.GetPtData(json: TJsonObject; dict: Boolean = false);
begin
  if (dict) then
    json.O['areas']
  else
    json.A['areas'];

  for var area: TArea in Self.db do
  begin
    try
      if (dict) then
        Area.GetPtData(json.O['areas'].O[Area.id])
      else
        Area.GetPtData(json.A['areas'].AddObject);
    except
      on E: Exception do
        PTUtils.PtErrorToJson(json.A['errors'].AddObject, 500, 'Chyba pri nacitani oblasti rizeni ' + Area.id,
          E.Message);
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

Areas := TAreas.Create();

finalization

FreeAndNil(Areas);

end.
