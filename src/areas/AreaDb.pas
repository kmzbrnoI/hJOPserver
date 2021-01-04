unit AreaDb;

{ TAreas class is a databse of areas (TArea). }

interface

uses Area, IniFiles, SysUtils, Classes, COmCtrls, IdContext,
     StdCtrls, Generics.Collections, JsonDataObjects;

type
  TAreas = class
   private const
     _SECT_OR = 'OR';

   private
     db: TObjectList<TArea>;
     fstat_filename: string;

      function GetORCnt(): Integer;

   public
      constructor Create();
      destructor Destroy(); override;

      procedure LoadData(const filename: string; const stat_filename: string);
      procedure SaveStatus(const filename: string);

      function Get(index: Integer): TArea; overload;
      function Get(id: string): TArea; overload;

      function ParseORs(str: string): TList<TArea>;
      procedure RCSFail(addr: integer); // je vyvolano pri vypadku RCS modulu, resi zobrazeni chyby do panelu v OR

      procedure Update();
      procedure DisconnectPanels();
      procedure SendORList(Context: TIdContext); // odesle seznam vsech OR na spojeni \Context

      procedure FillCB(CB: TComboBox; selected: TArea);  // naplni ComboBox seznamem oblasti rizeni
      procedure InitOsv();
      procedure BroadcastBottomError(err: string; tech: string; min_rights: TAreaRights = read);
                                                                                // broadcast chybove hlasky, ktera ma jit jen panelum,
                                                                                // kde alespon jeden je minimalne opravneni min_rights
      procedure BroadcastPlaySound(sound_code: Integer; loop: Boolean = false; min_rights: TAreaRights = read);

      function GetEnumerator(): TEnumerator<TArea>;

      procedure GetPtData(json: TJsonObject; dict: Boolean = false);

      property Items[index : integer] : TArea read Get; default;
      property Count: Integer read GetORCnt;

      property status_filename: string read fstat_filename;
  end;

var
  Areas: TAreas;

implementation

uses Logging, TCPServerPanel, THVDatabase, appEv, FileSystem, PTUtils;

////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////

//nacitani OR a vytvareni vsech OR
procedure TAreas.LoadData(const filename: string; const stat_filename: string);
var ini, ini_stat: TMemIniFile;
    areas: TStrings;
    i: Integer;
    area: TArea;
begin
 writelog('Načítám stanice - '+filename, WR_DATA);
 Self.fstat_filename := stat_filename;

 if (not FileExists(filename)) then
   raise EFileNotFound.Create('Soubor se stanicemi neexistuje - '+filename);

 Self.db.Clear();
 ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 ini_stat := TMemIniFile.Create(stat_filename, TEncoding.UTF8);
 areas := TStringList.Create();

 try
   ini.ReadSection(_SECT_OR, areas);

   for i := 0 to areas.Count-1 do
    begin
     area := TArea.Create(i+1);
     try
       area.LoadData(ini.ReadString(_SECT_OR, areas[i], ''));
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
   for i := 0 to Self.db.Count-1 do
     Self.db[i].index := i;
 finally
   areas.Free();
   ini.Free();
   ini_stat.Free();
 end;

 writelog('Načteno '+IntToStr(Self.db.Count)+' stanic', WR_DATA);
end;

////////////////////////////////////////////////////////////////////////////////
// ukladani stavu vsech oblasti rizeni

procedure TAreas.SaveStatus(const filename: string);
var ini: TMemIniFile;
    area: TArea;
begin
 ini := TMemIniFile.Create(filename, TEncoding.UTF8);

 for area in Self.db do
  begin
   try
     area.SaveStat(ini, area.id);
   except
     on E: Exception do
       AppEvents.LogException(E, 'Ukladani stavu OR ' + area.id);
   end;
  end;

 ini.UpdateFile();
 ini.Free();
end;

////////////////////////////////////////////////////////////////////////////////

//parsing OR stringu
function TAreas.ParseORs(str: string): TList<TArea>;
var parsed: TStrings;
    areaid: string;
begin
 parsed := TStringList.Create();
 try
   ExtractStrings(['|'],[], PChar(str), parsed);

   Result := TList<TArea>.Create();
   for areaid in parsed do
     Result.Add(Self.Get(areaid));
 finally
   parsed.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TAreas.Get(index: Integer): TArea;
begin
 Result := Self.db[index];
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAreas.Update();
var i: Integer;
begin
 for i := 0 to Self.db.Count-1 do
   Self.db[i].Update();
end;

procedure TAreas.DisconnectPanels();
var i: Integer;
begin
 for i := 0 to Self.db.Count-1 do
   Self.db[i].DisconnectPanels();

 // vymazeme vsechny otevrene regulatory u klientu
 for i := 0 to _MAX_ADDR-1 do
   if (Assigned(HVDb[i])) then
     HVDb[i].state.regulators.Clear();
end;

procedure TAreas.SendORList(Context: TIdContext);
var i: Integer;
    str: string;
begin
 str := '-;OR-LIST;';
 for i := 0 to Self.db.Count-1 do
   str := str + '[' + Self.db[i].id + ',' + Self.db[i].Name + ']';

 PanelServer.SendLn(Context, str);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAreas.RCSFail(addr: integer);
var area: TArea;
begin
 for area in Self.db do
   area.RCSFail(addr);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAreas.FillCB(CB: TComboBox; selected: TArea);
var i: Integer;
begin
 CB.Clear();
 for i := 0 to Self.db.Count-1 do
  begin
   CB.Items.Add(Self.db[i].Name);
   if (Self.db[i] = selected) then
    CB.ItemIndex := i;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TAreas.GetORCnt(): Integer;
begin
 Result := Self.db.Count;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAreas.InitOsv();
var areas: TArea;
begin
 for areas in Self.db do
   areas.OsvInit();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAreas.BroadcastBottomError(err: string; tech: string; min_rights: TAreaRights = read);
var areas: TArea;
    connected: TAreaPanel;
    clients: TDictionary<TIdContext, Boolean>; // set
    client: TIdContext;
begin
 clients := TDictionary<TIdContext, Boolean>.Create();
 for areas in Self.db do
   for connected in areas.Connected do
     if (connected.Rights >= min_rights) then
       clients.AddOrSetValue(connected.Panel, true);

 for client in clients.Keys do
   PanelServer.BottomError(client, err, '-', tech);

 clients.Free();
end;

procedure TAreas.BroadcastPlaySound(sound_code: Integer; loop: Boolean = false; min_rights: TAreaRights = read);
var area: TArea;
    connected: TAreaPanel;
    clients: TDictionary<TIdContext, Boolean>; // set
    client: TIdContext;
begin
 clients := TDictionary<TIdContext, Boolean>.Create();
 for area in Self.db do
   for connected in area.Connected do
     if (connected.Rights >= min_rights) then
       clients.AddOrSetValue(connected.Panel, true);

 for client in clients.Keys do
   PanelServer.PlaySound(client, sound_code, loop);

 clients.Free();
end;

////////////////////////////////////////////////////////////////////////////////

function TAreas.Get(id: string): TArea;
var left, right, mid: Integer;
begin
 left := 0;
 right := Self.db.Count-1;

 while (left <= right) do
  begin
   mid := (left + right) div 2;
   if (Self.db[mid].id = id) then Exit(Self.db[mid]);

   if (CompareStr(id, Self.db[mid].id, loUserLocale) < 0) then
     right := mid - 1
   else
     left := mid + 1;
  end;
 Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

function TAreas.GetEnumerator(): TEnumerator<TArea>;
begin
 Result := Self.db.GetEnumerator();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAreas.GetPtData(json: TJsonObject; dict: Boolean = false);
var area: TArea;
begin
 if (dict) then
   json.O['areas']
 else
   json.A['areas'];

 for area in Self.db do
  begin
   try
     if (dict) then
       area.GetPtData(json.O['areas'].O[area.id])
     else
       area.GetPtData(json.A['areas'].AddObject);
   except
     on E: Exception do
       PTUtils.PtErrorToJson(json.A['errors'].AddObject,
        '500', 'Chyba pri nacitani oblasti rizeni '+area.id, E.Message);
   end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  Areas := TAreas.Create();
finalization
  FreeAndNil(Areas);

end.//unit
