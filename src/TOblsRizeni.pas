unit TOblsRizeni;

{
  Trida TORs sdruzuje oblasti rizeni do databaze.
}

interface

uses TOblRizeni, IniFiles, SysUtils, Classes, COmCtrls, IdContext,
      StdCtrls, Generics.Collections;

type
  TORs = class
   private const
     _SECT_OR = 'OR';                                                           // sekce ini souboru .spnl, ve ktere jsou ulozeny oblasti rizeni

   private
     ORsDatabase:TObjectList<TOR>;                                                    // databaze oblasti rizeni
     fstat_filename:string;

     function GetORCnt():Integer;                                               // vrati pocet OR

   public
      constructor Create();
      destructor Destroy(); override;

      procedure LoadData(const filename:string; const stat_filename:string);
      procedure SaveStatus(const filename:string);

      function GetORIndex(const id:string):Integer;
      function ParseORs(str:string):TList<TOR>;                                 // parsuje seznam oblasti rizeni
      procedure RCSFail(addr:integer);                                          // je vyvolano pri vypadku RCS modulu, resi zobrazeni chyby do panelu v OR
      function GetORByIndex(index:Integer;var obl:TOR):Byte;
      function GetORNameByIndex(index:Integer):string;
      function GetORIdByIndex(index:Integer):string;
      function GetORShortNameByIndex(index:Integer):string;
      function GetORById(id:string):TOR;

      procedure Update();                                                       // aktualizuje stav OR
      procedure DisconnectPanels();                                             // odpoji vsechny panely dane OR
      procedure SendORList(Context:TIdContext);                                 // odesle seznam vsech OR na spojeni \Context

      procedure FillCB(CB:TComboBox; selected:TOR);                             // naplni ComboBox seznamem oblasti rizeni

      procedure InitOsv();

      procedure BroadcastBottomError(err:string; tech:string; min_rights:TORControlRights = read);
                                                                                // broadcast chybove hlasky, ktera ma jit jen panelum,
                                                                                // kde alespon jeden je minimalne opravneni min_rights
      procedure BroadcastPlaySound(sound_code:Integer; loop:boolean = false; min_rights:TORControlRights = read);

      property Count:Integer read GetORCnt;                                     // vrati seznam oblasti rizeni
      property status_filename:string read fstat_filename;
  end;//TORs

var
  ORs:TORs;

implementation

uses Prevody, Logging, TCPServerOR, THVDatabase, appEv, FileSystem;

////////////////////////////////////////////////////////////////////////////////

constructor TORs.Create();
begin
 inherited;
 Self.ORsDatabase := TObjectList<TOR>.Create();
end;//ctor

destructor TORs.Destroy();
begin
 Self.ORsDatabase.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

//nacitani OR a vytvareni vsech OR
procedure TORs.LoadData(const filename:string; const stat_filename:string);
var ini, ini_stat:TMemIniFile;
    oblasti:TStrings;
    i:Integer;
    OblR:TOR;
begin
 writelog('Načítám stanice - '+filename,WR_DATA);
 Self.fstat_filename := stat_filename;

 if (not FileExists(filename)) then
   raise EFileNotFound.Create('Soubor se stanicemi neexistuje - '+filename);

 Self.ORsDatabase.Clear();
 ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 ini_stat := TMemIniFile.Create(stat_filename, TEncoding.UTF8);
 oblasti := TStringList.Create();

 try
   ini.ReadSection(_SECT_OR, oblasti);

   for i := 0 to oblasti.Count-1 do
    begin
     OblR := TOR.Create(i+1);
     try
       OblR.LoadData(ini.ReadString(_SECT_OR, oblasti[i], ''));
       OblR.LoadStat(ini_stat, OblR.id);
       Self.ORsDatabase.Add(OblR);
     except
       on E:Exception do
        begin
         AppEvents.LogException(E, 'Nacitam oblast rizeni ' + IntToStr(i));
         OblR.Free();
        end;
     end;
    end;

   Self.ORsDatabase.Sort(TOR.NameComparer());
   for i := 0 to Self.ORsDatabase.Count-1 do
     Self.ORsDatabase[i].index := i+1;
 finally
   oblasti.Free();
   ini.Free();
   ini_stat.Free();
 end;

 writelog('Načteno '+IntToStr(Self.ORsDatabase.Count)+' stanic',WR_DATA);
end;

////////////////////////////////////////////////////////////////////////////////
// ukladani stavu vsech oblasti rizeni

procedure TORs.SaveStatus(const filename:string);
var ini:TMemIniFile;
    oblr:TOR;
begin
 ini := TMemIniFile.Create(filename, TEncoding.UTF8);

 for oblr in Self.ORsDatabase do
  begin
   try
     oblr.SaveStat(ini, oblr.id);
   except
     on E:Exception do
       AppEvents.LogException(E, 'Ukladani stavu OR ' + oblr.id);
   end;
  end;

 ini.UpdateFile();
 ini.Free();
end;

////////////////////////////////////////////////////////////////////////////////

//vrati index OR s danym ID (index v databazi ORs)
function TORs.GetORIndex(const id:string):Integer;
var i:Integer;
begin
 for i := 0 to Self.ORsDatabase.Count-1 do
   if (Self.ORsDatabase[i].id = id) then
     Exit(i);

 Result := -1;
end;

////////////////////////////////////////////////////////////////////////////////

//parsing OR stringu
function TORs.ParseORs(str:string):TList<TOR>;
var parsed:TStrings;
    oblr:string;
begin
 parsed := TStringList.Create();
 try
   ExtractStrings(['|'],[],PChar(str),parsed);

   Result := TList<TOR>.Create();
   for oblr in parsed do
     Result.Add(Self.ORsDatabase[Self.GetORIndex(oblr)]);
 finally
   parsed.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TORs.GetORByIndex(index:Integer; var obl:TOR):Byte;
begin
 if ((index < 0) or (index >= Self.ORsDatabase.Count)) then Exit(1);
 obl := Self.ORsDatabase[index];
 Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////

function TORs.GetORNameByIndex(index:Integer):string;
begin
 if ((index < 0) or (index >= Self.ORsDatabase.Count)) then
   Exit('## OR s timto indexem neexistuje ##');

 Result := Self.ORsDatabase[index].Name;
end;

function TORs.GetORIdByIndex(index:Integer):string;
begin
 if ((index < 0) or (index >= Self.ORsDatabase.Count)) then
   Exit('## OR s timto indexem neexistuje ##');

 Result := Self.ORsDatabase[index].id;
end;

function TORs.GetORShortNameByIndex(index:Integer):string;
begin
 if ((index < 0) or (index >= Self.ORsDatabase.Count)) then
   Exit('## OR s timto indexem neexistuje ##');

 Result := Self.ORsDatabase[index].ShortName;
end;

procedure TORs.Update();
var i:Integer;
begin
 for i := 0 to Self.ORsDatabase.Count-1 do
   Self.ORsDatabase[i].Update();
end;

procedure TORs.DisconnectPanels();
var i:Integer;
begin
 for i := 0 to Self.ORsDatabase.Count-1 do
   Self.ORsDatabase[i].DisconnectPanels();

 // vymazeme vsechny otevrene regulatory u klientu
 for i := 0 to _MAX_ADDR-1 do
  if (Assigned(HVDb.HVozidla[i])) then
    HVDb.HVozidla[i].Stav.regulators.Clear();
end;

procedure TORs.SendORList(Context:TIdContext);
var i:Integer;
    str:string;
begin
 str := '-;OR-LIST;';
 for i := 0 to Self.ORsDatabase.Count-1 do
   str := str + '[' + Self.ORsDatabase[i].id + ',' + Self.ORsDatabase[i].Name + ']';

 ORTCPServer.SendLn(Context, str);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORs.RCSFail(addr:integer);
var OblR:TOR;
begin
 for OblR in Self.ORsDatabase do
   OblR.RCSFail(addr);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORs.FillCB(CB:TComboBox; selected:TOR);
var i:Integer;
begin
 CB.Clear();
 for i := 0 to Self.ORsDatabase.Count-1 do
  begin
   CB.Items.Add(Self.ORsDatabase[i].Name);
   if (Self.ORsDatabase[i] = selected) then
    CB.ItemIndex := i;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TORs.GetORCnt():Integer;
begin
 Result := Self.ORsDatabase.Count;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORs.InitOsv();
var oblr:TOR;
begin
 for oblr in Self.ORsDatabase do oblr.InitOsv();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORs.BroadcastBottomError(err:string; tech:string; min_rights:TORControlRights = read);
var OblR:TOR;
    connected:TORPanel;
    clients:TDictionary<TIdContext,Boolean>; // set
    client:TIdContext;
begin
 clients := TDictionary<TIdContext,Boolean>.Create();
 for OblR in Self.ORsDatabase do
   for connected in OblR.Connected do
     if (connected.Rights >= min_rights) then
       clients.AddOrSetValue(connected.Panel, true);

 for client in clients.Keys do
   ORTCPServer.BottomError(client, err, '-', tech);

 clients.Free();
end;

procedure TORs.BroadcastPlaySound(sound_code:Integer; loop:boolean = false; min_rights:TORControlRights = read);
var OblR:TOR;
    connected:TORPanel;
    clients:TDictionary<TIdContext,Boolean>; // set
    client:TIdContext;
begin
 clients := TDictionary<TIdContext,Boolean>.Create();
 for OblR in Self.ORsDatabase do
   for connected in OblR.Connected do
     if (connected.Rights >= min_rights) then
       clients.AddOrSetValue(connected.Panel, true);

 for client in clients.Keys do
   ORTCPServer.PlaySound(client, sound_code, loop);

 clients.Free();
end;

////////////////////////////////////////////////////////////////////////////////

function TORs.GetORById(id:string):TOR;
var OblR:TOR;
begin
 for OblR in Self.ORsDatabase do
   if (OblR.id = id) then
     Exit(OblR);
 Exit(nil);
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  ORs := TORs.Create();
finalization
  FreeAndNil(ORs);

end.//unit
