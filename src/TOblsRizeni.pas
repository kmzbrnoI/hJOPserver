unit TOblsRizeni;

//tato unita implementuje tridu TORs, ktera sdruzuje oblasti rizeni

interface

uses TOblRizeni, IniFiles, SysUtils, Classes, RPConst, COmCtrls, IdContext,
      StdCtrls;

const
  _MAX_OR = 256;

type
  //recordy vyizivany pri externich implementacich navaznosti na OR (napriklad u technologickych bloku)
  // vyuziva se i u TCP serveru - kazde spojeni si pamatuje, jake jsou na nem oblasti rizeni
  TORsRef = record
    ORs:array [0.._MAX_ORREF-1] of TOR;
    Cnt:Integer;
  end;

  TORsDatabase = record
    Data:array [0.._MAX_OR-1] of TOR;
    Cnt:Integer;
  end;

  TORs = class
   private const
     _SECT_OR = 'OR';
   private
     ORsDatabase:TORsDatabase;    //databaze oblasti rizeni

     procedure FreeORs();

   public
      constructor Create();
      destructor Destroy(); override;

      function LoadData(const filename:string):Byte;

      function GetORIndex(const id:string):Integer;

      function ParseORs(str:string):TORsRef;

      procedure MTBFail(addr:integer);

      function GetORByIndex(index:Integer;var obl:TOR):Byte;
      function GetORNameByIndex(index:Integer):string;
      function GetORIdByIndex(index:Integer):string;
      function GetORShortNameByIndex(index:Integer):string;

      procedure Update();
      procedure DisconnectPanels();
      procedure SendORList(Context:TIdContext);

      procedure FillCB(CB:TComboBox; selected:TOR);

      property ORcnt:Integer read ORsDatabase.Cnt;
  end;//TORs

var
  ORs:TORs;

implementation

uses Prevody, Logging, TCPServerOR, THVDatabase;

////////////////////////////////////////////////////////////////////////////////

constructor TORs.Create();
begin
 inherited Create();
end;//ctor

destructor TORs.Destroy();
begin
 Self.FreeORs();

 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

//nacitani OR a vytvareni vsech OR
function TORs.LoadData(const filename:string):Byte;
var ini:TMemIniFile;
    oblasti:TStrings;
    i:Integer;
begin
 if (not FileExists(filename)) then
  begin
   writelog('Soubor se stanicemi neexistuje - '+filename,WR_ERROR);
   Exit(1);
  end;

 writelog('Naèítám stanice - '+filename,WR_DATA);

 ini := TMemIniFile.Create(filename);
 oblasti := TStringList.Create();

 ini.ReadSection(_SECT_OR, oblasti);

 Self.ORsDatabase.Cnt := oblasti.Count;
 for i := 0 to oblasti.Count-1 do
  begin
   Self.ORsDatabase.Data[i] := TOR.Create(i);
   if (Self.ORsDatabase.Data[i].LoadData(ini.ReadString(_SECT_OR, oblasti[i], '')) <> 0) then
    begin
     Result := 2;
     Exit;
    end;
  end;//for i

 oblasti.Free();
 ini.Free();
 Result := 0;

 writelog('Naèteno '+IntToStr(Self.ORsDatabase.Cnt)+' stanic',WR_DATA);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//smazani databaze a regulerni zniceni trid v teto databazi
procedure TORs.FreeORs();
var i:Integer;
begin
 for i := 0 to Self.ORsDatabase.Cnt-1 do
   if (Assigned(Self.ORsDatabase.Data[i])) then FreeAndNil(Self.ORsDatabase.Data[i]);

 Self.ORsDatabase.Cnt := 0;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//vrati index OR s danym ID (index v databazi ORs)
function TORs.GetORIndex(const id:string):Integer;
var i:Integer;
begin
 for i := 0 to Self.ORsDatabase.Cnt-1 do
   if (Self.ORsDatabase.Data[i].id = id) then
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
    Result.ORs[i] := Self.ORsDatabase.Data[Self.GetORIndex(parsed[i])];

 parsed.Free();
end;//function

////////////////////////////////////////////////////////////////////////////////

function TORs.GetORByIndex(index:Integer; var obl:TOR):Byte;
begin
 if ((index < 0) or (index >= Self.ORsDatabase.Cnt)) then Exit(1);
 obl := Self.ORsDatabase.Data[index];
 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TORs.GetORNameByIndex(index:Integer):string;
begin
 if ((index < 0) or (index >= Self.ORsDatabase.Cnt)) then
   Exit('## OR s timto indexem neexistuje ##');

 Result := Self.ORsDatabase.Data[index].Name;
end;//function

function TORs.GetORIdByIndex(index:Integer):string;
begin
 if ((index < 0) or (index >= Self.ORsDatabase.Cnt)) then
   Exit('## OR s timto indexem neexistuje ##');

 Result := Self.ORsDatabase.Data[index].id;
end;//function

function TORs.GetORShortNameByIndex(index:Integer):string;
begin
 if ((index < 0) or (index >= Self.ORsDatabase.Cnt)) then
   Exit('## OR s timto indexem neexistuje ##');

 Result := Self.ORsDatabase.Data[index].ShortName;
end;//function

procedure TORs.Update();
var i:Integer;
begin
 for i := 0 to Self.ORsDatabase.Cnt-1 do
   Self.ORsDatabase.Data[i].Update();
end;//procedure

procedure TORs.DisconnectPanels();
var i:Integer;
begin
 for i := 0 to Self.ORsDatabase.Cnt-1 do
   Self.ORsDatabase.Data[i].DisconnectPanels();

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
 for i := 0 to Self.ORsDatabase.Cnt-1 do
   str := str + '{' + Self.ORsDatabase.Data[i].id + ',' + Self.ORsDatabase.Data[i].Name + '}';

 ORTCPServer.SendLn(Context, str);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORs.MTBFail(addr:integer);
var i:Integer;
begin
 for i := 0 to Self.ORsDatabase.Cnt-1 do
  Self.ORsDatabase.Data[i].MTBFail(addr); 
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORs.FillCB(CB:TComboBox; selected:TOR);
var i:Integer;
begin
 CB.Clear();
 for i := 0 to Self.ORsDatabase.Cnt-1 do
  begin
   CB.Items.Add(Self.ORsDatabase.Data[i].Name);
   if (Self.ORsDatabase.Data[i] = selected) then
    CB.ItemIndex := i;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

end.//unit
