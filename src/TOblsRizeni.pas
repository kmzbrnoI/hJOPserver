unit TOblsRizeni;

{
  Trida TORs sdruzuje oblasti rizeni do databaze.
}

interface

uses TOblRizeni, IniFiles, SysUtils, Classes, RPConst, COmCtrls, IdContext,
      StdCtrls, Generics.Collections;

type
  //recordy vyuzivany pri externich implementacich navaznosti na OR (napriklad u technologickych bloku)
  // vyuziva se i u TCP serveru - kazde spojeni si pamatuje, jake jsou na nem oblasti rizeni
  TORsRef = record
    ORs:array [0.._MAX_ORREF-1] of TOR;
    Cnt:Integer;
  end;

  TORs = class
   private const
     _SECT_OR = 'OR';                                                           // sekce ini souboru .spnl, ve ktere jsou ulozeny oblasti rizeni

   private
     ORsDatabase:TList<TOR>;                                                    // databaze oblasti rizeni
     fstat_filename:string;

     procedure FreeORs();                                                       // zniceni a vymazani vsech OR
     function GetORCnt():Integer;                                               // vrati pocet OR

   public
      constructor Create();
      destructor Destroy(); override;

      procedure LoadData(const filename:string; const stat_filename:string);
      procedure SaveStatus(const filename:string);

      function GetORIndex(const id:string):Integer;
      function ParseORs(str:string):TORsRef;                                    // parsuje seznam oblasti rizeni
      procedure MTBFail(addr:integer);                                          // je vyvolano pri vypadku MTB modulu, resi zobrazeni chyby do panelu v OR
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

      property Count:Integer read GetORCnt;                                     // vrati seznam oblasti rizeni
      property status_filename:string read fstat_filename;
  end;//TORs

var
  ORs:TORs;

implementation

uses Prevody, Logging, TCPServerOR, THVDatabase, appEv;

////////////////////////////////////////////////////////////////////////////////

constructor TORs.Create();
begin
 inherited;
 Self.ORsDatabase := TList<TOR>.Create();
end;//ctor

destructor TORs.Destroy();
begin
 Self.FreeORs();
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
 writelog('Naèítám stanice - '+filename,WR_DATA);

 if (not FileExists(filename)) then
   raise Exception.Create('Soubor se stanicemi neexistuje - '+filename);

 Self.ORsDatabase.Clear();
 ini := TMemIniFile.Create(filename);
 ini_stat := TMemIniFile.Create(stat_filename);
 Self.fstat_filename := stat_filename;
 oblasti := TStringList.Create();

 ini.ReadSection(_SECT_OR, oblasti);

 for i := 0 to oblasti.Count-1 do
  begin
   OblR := TOR.Create(i);
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
  end;//for i

 oblasti.Free();
 ini.Free();
 ini_stat.Free();

 writelog('Naèteno '+IntToStr(Self.ORsDatabase.Count)+' stanic',WR_DATA);
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// ukladani stavu vsech oblasti rizeni

procedure TORs.SaveStatus(const filename:string);
var ini:TMemIniFile;
    oblr:TOR;
begin
 ini := TMemIniFile.Create(filename);

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

//smazani databaze a regulerni zniceni trid v teto databazi
procedure TORs.FreeORs();
var i:Integer;
begin
 for i := 0 to Self.ORsDatabase.Count-1 do
   if (Assigned(Self.ORsDatabase[i])) then Self.ORsDatabase[i].Free();
 Self.ORsDatabase.Clear();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//vrati index OR s danym ID (index v databazi ORs)
function TORs.GetORIndex(const id:string):Integer;
var i:Integer;
begin
 for i := 0 to Self.ORsDatabase.Count-1 do
   if (Self.ORsDatabase[i].id = id) then
     Exit(i);

 Result := -1;
end;//function

////////////////////////////////////////////////////////////////////////////////

//parsing OR stringu
function TORs.ParseORs(str:string):TORsRef;
var parsed:TStrings;
    i:Integer;
begin
 parsed := TStringList.Create();

 ExtractStrings(['|'],[],PChar(str),parsed);

 Result.Cnt := parsed.Count;
 for i := 0 to parsed.Count-1 do
    Result.ORs[i] := Self.ORsDatabase[Self.GetORIndex(parsed[i])];

 parsed.Free();
end;//function

////////////////////////////////////////////////////////////////////////////////

function TORs.GetORByIndex(index:Integer; var obl:TOR):Byte;
begin
 if ((index < 0) or (index >= Self.ORsDatabase.Count)) then Exit(1);
 obl := Self.ORsDatabase[index];
 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TORs.GetORNameByIndex(index:Integer):string;
begin
 if ((index < 0) or (index >= Self.ORsDatabase.Count)) then
   Exit('## OR s timto indexem neexistuje ##');

 Result := Self.ORsDatabase[index].Name;
end;//function

function TORs.GetORIdByIndex(index:Integer):string;
begin
 if ((index < 0) or (index >= Self.ORsDatabase.Count)) then
   Exit('## OR s timto indexem neexistuje ##');

 Result := Self.ORsDatabase[index].id;
end;//function

function TORs.GetORShortNameByIndex(index:Integer):string;
begin
 if ((index < 0) or (index >= Self.ORsDatabase.Count)) then
   Exit('## OR s timto indexem neexistuje ##');

 Result := Self.ORsDatabase[index].ShortName;
end;//function

procedure TORs.Update();
var i:Integer;
begin
 for i := 0 to Self.ORsDatabase.Count-1 do
   Self.ORsDatabase[i].Update();
end;//procedure

procedure TORs.DisconnectPanels();
var i:Integer;
begin
 for i := 0 to Self.ORsDatabase.Count-1 do
   Self.ORsDatabase[i].DisconnectPanels();

 // vymazeme vsechny otevrene regulatory u klientu
 for i := 0 to _MAX_ADDR-1 do
  if (Assigned(HVDb.HVozidla[i])) then
    HVDb.HVozidla[i].Stav.regulators.Clear();
end;//procedure

procedure TORs.SendORList(Context:TIdContext);
var i:Integer;
    str:string;
begin
 str := '-;OR-LIST;';
 for i := 0 to Self.ORsDatabase.Count-1 do
   str := str + '[' + Self.ORsDatabase[i].id + ',' + Self.ORsDatabase[i].Name + ']';

 ORTCPServer.SendLn(Context, str);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORs.MTBFail(addr:integer);
var i:Integer;
begin
 for i := 0 to Self.ORsDatabase.Count-1 do
  Self.ORsDatabase[i].MTBFail(addr);
end;//procedure

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
end;//procedure

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

end.//unit
