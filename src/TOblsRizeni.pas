unit TOblsRizeni;

{
  Trida TORs sdruzuje oblasti rizeni do databaze.
}

interface

uses Area, IniFiles, SysUtils, Classes, COmCtrls, IdContext,
     StdCtrls, Generics.Collections;

type
  TORs = class
   private const
     _SECT_OR = 'OR';                                                           // sekce ini souboru .spnl, ve ktere jsou ulozeny oblasti rizeni

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

      function ParseORs(str: string): TList<TArea>;                                 // parsuje seznam oblasti rizeni
      procedure RCSFail(addr: integer);                                          // je vyvolano pri vypadku RCS modulu, resi zobrazeni chyby do panelu v OR

      procedure Update();                                                       // aktualizuje stav OR
      procedure DisconnectPanels();                                             // odpoji vsechny panely dane OR
      procedure SendORList(Context: TIdContext);                                 // odesle seznam vsech OR na spojeni \Context

      procedure FillCB(CB: TComboBox; selected: TArea);                             // naplni ComboBox seznamem oblasti rizeni
      procedure InitOsv();
      procedure BroadcastBottomError(err: string; tech: string; min_rights: TAreaRights = read);
                                                                                // broadcast chybove hlasky, ktera ma jit jen panelum,
                                                                                // kde alespon jeden je minimalne opravneni min_rights
      procedure BroadcastPlaySound(sound_code: Integer; loop: Boolean = false; min_rights: TAreaRights = read);

      function GetEnumerator(): TEnumerator<TArea>;
      property Items[index : integer] : TArea read Get; default;
      property Count: Integer read GetORCnt;

      property status_filename: string read fstat_filename;
  end;

var
  ORs: TORs;

implementation

uses Logging, TCPServerOR, THVDatabase, appEv, FileSystem;

////////////////////////////////////////////////////////////////////////////////

constructor TORs.Create();
begin
 inherited;
 Self.db := TObjectList<TArea>.Create(TArea.IdComparer());
end;

destructor TORs.Destroy();
begin
 Self.db.Free();
 inherited Destroy();
end;

////////////////////////////////////////////////////////////////////////////////

//nacitani OR a vytvareni vsech OR
procedure TORs.LoadData(const filename: string; const stat_filename: string);
var ini, ini_stat: TMemIniFile;
    oblasti: TStrings;
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
 oblasti := TStringList.Create();

 try
   ini.ReadSection(_SECT_OR, oblasti);

   for i := 0 to oblasti.Count-1 do
    begin
     area := TArea.Create(i+1);
     try
       area.LoadData(ini.ReadString(_SECT_OR, oblasti[i], ''));
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
   oblasti.Free();
   ini.Free();
   ini_stat.Free();
 end;

 writelog('Načteno '+IntToStr(Self.db.Count)+' stanic', WR_DATA);
end;

////////////////////////////////////////////////////////////////////////////////
// ukladani stavu vsech oblasti rizeni

procedure TORs.SaveStatus(const filename: string);
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
function TORs.ParseORs(str: string): TList<TArea>;
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

function TORs.Get(index: Integer): TArea;
begin
 Result := Self.db[index];
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORs.Update();
var i: Integer;
begin
 for i := 0 to Self.db.Count-1 do
   Self.db[i].Update();
end;

procedure TORs.DisconnectPanels();
var i: Integer;
begin
 for i := 0 to Self.db.Count-1 do
   Self.db[i].DisconnectPanels();

 // vymazeme vsechny otevrene regulatory u klientu
 for i := 0 to _MAX_ADDR-1 do
  if (Assigned(HVDb[i])) then
    HVDb[i].Stav.regulators.Clear();
end;

procedure TORs.SendORList(Context: TIdContext);
var i: Integer;
    str: string;
begin
 str := '-;OR-LIST;';
 for i := 0 to Self.db.Count-1 do
   str := str + '[' + Self.db[i].id + ',' + Self.db[i].Name + ']';

 ORTCPServer.SendLn(Context, str);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORs.RCSFail(addr: integer);
var area: TArea;
begin
 for area in Self.db do
   area.RCSFail(addr);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORs.FillCB(CB: TComboBox; selected: TArea);
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

function TORs.GetORCnt(): Integer;
begin
 Result := Self.db.Count;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORs.InitOsv();
var areas: TArea;
begin
 for areas in Self.db do
   areas.OsvInit();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORs.BroadcastBottomError(err: string; tech: string; min_rights: TAreaRights = read);
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
   ORTCPServer.BottomError(client, err, '-', tech);

 clients.Free();
end;

procedure TORs.BroadcastPlaySound(sound_code: Integer; loop: Boolean = false; min_rights: TAreaRights = read);
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
   ORTCPServer.PlaySound(client, sound_code, loop);

 clients.Free();
end;

////////////////////////////////////////////////////////////////////////////////

function TORs.Get(id: string): TArea;
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

function TORs.GetEnumerator(): TEnumerator<TArea>;
begin
 Result := Self.db.GetEnumerator();
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  ORs := TORs.Create();
finalization
  FreeAndNil(ORs);

end.//unit
