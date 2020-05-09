unit SprDb;

// databaze souprav

interface

uses SysUtils, Souprava, IniFiles, Classes, Windows, Forms, Trakce;

const
  _MAX_SPR = 128;

type

  TSprDb = class
   private
      ffilename:string;

      procedure FreeSpr();

      function GetCount():Integer;
      function GetItem(index:Integer):TSouprava;
      function GetEmptySpaceForSpr():Integer;

   public
      soupravy:array [0.._MAX_SPR] of TSouprava;

      constructor Create();
      destructor Destroy(); override;

      procedure LoadData(const filename:string);
      procedure SaveData(const filename:string);

      procedure AddSprFromPanel(spr:TStrings; usek:TObject; OblR:TObject; sprUsekIndex:Integer; ok: TCb; err: TCb);
      procedure RemoveSpr(index:Integer);

      function GetSprNameByIndex(index:Integer):string;
      function GetSprIndexByName(name:string):Integer;

      procedure UpdateFront();
      procedure StopAllSpr();
      procedure ClearPOdj();

      property filename:string read ffilename;

      property Items[index : integer] : TSouprava read GetItem; default;
      property count:Integer read GetCount;

  end;//TSprDb

var
  Soupravy : TSprDb;

implementation

uses Logging, DataSpr, TBloky, TBlokUsek, DataHV, appEv, TBlok,
     TCPServerOR;

////////////////////////////////////////////////////////////////////////////////

constructor TSprDb.Create();
var i:Integer;
begin
 inherited;

 for i := 0 to _MAX_SPR-1 do
   Self.soupravy[i] := nil;
end;//ctor

destructor TSprDb.Destroy();
begin
 Self.FreeSpr();
 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TSprDb.FreeSpr();
var i:Integer;
begin
 for i := 0 to _MAX_SPR-1 do
  if (Assigned(Self.soupravy[i])) then
   FreeAndNil(Self.soupravy[i]);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSprDb.LoadData(const filename:string);
var ini:TMemIniFile;
    i:Integer;
    sections:TStrings;
begin
 writelog('Načítám soupravy: '+filename, WR_DATA);
 Self.ffilename := filename;

 ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 sections := TStringList.Create();

 try
   ini.ReadSections(sections);

   Self.FreeSpr();

   for i := 0 to sections.Count-1 do
     Self.soupravy[i] := TSouprava.Create(ini, sections[i], i);

   writelog('Načteno '+IntToStr(sections.Count)+' souprav', WR_DATA);
 finally
   FreeAndNil(ini);
   FreeAndNil(sections);
 end;

 SprTableData.LoadToTable();
 HVTableData.LoadToTable();
end;

procedure TSprDb.SaveData(const filename:string);
var ini:TMemIniFile;
    i:Integer;
begin
 writelog('Ukládám soupravy: '+filename, WR_DATA);

 if (FileExists(filename)) then
   DeleteFile(PChar(filename));
 ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 try
   for i := 0 to _MAX_SPR-1 do
     if (Assigned(Self.soupravy[i])) then
       Self.soupravy[i].SaveToFile(ini, IntToStr(i));
   ini.UpdateFile();
 finally
   FreeAndNil(ini);
 end;

 writelog('Uloženo '+IntToStr(Self.Count)+' souprav', WR_DATA);
end;

////////////////////////////////////////////////////////////////////////////////

function TSprDb.GetCount():Integer;
var i:Integer;
begin
 Result := 0;
 for i := 0 to _MAX_SPR do
  if (Assigned(Self.soupravy[i])) then
   Inc(Result);
end;

////////////////////////////////////////////////////////////////////////////////

function TSprDb.GetSprNameByIndex(index:Integer):string;
begin
 if (index < 0) or (index >= _MAX_SPR) then Exit('-');

 if (Assigned(Self.soupravy[index])) then
  Result := Self.soupravy[index].name
 else
  Result := '-';
end;

////////////////////////////////////////////////////////////////////////////////

function TSprDb.GetSprIndexByName(name:string):Integer;
var i:Integer;
begin
 for i := 0 to _MAX_SPR-1 do
  if ((Assigned(Self.soupravy[i])) and (Self.soupravy[i].name = name)) then
   Exit(i);
 Exit(-1);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSprDb.AddSprFromPanel(spr:TStrings; Usek:TObject; OblR:TObject; sprUsekIndex:Integer; ok: TCb; err: TCb);
var i:Integer;
begin
 i := Self.GetEmptySpaceForSpr();

 try
  Self.soupravy[i] := TSouprava.Create(spr, Usek, i, OblR, ok, err);
  if (Assigned(Usek)) then          // toto musi byt tady, nikoliv v konstruktoru
   begin
    (Usek as TBlkUsek).AddSouprava(sprUsekIndex, i);
    (Usek as TBlkUsek).Change();    // volano kvuli aktualizaci dat
   end;
 except
  on E: Exception do
   begin
    FreeAndNil(Self.soupravy[i]);
    SprTableData.reload := true;
    raise Exception.Create(E.Message);
   end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSprDb.RemoveSpr(index:Integer);
begin
 if (not Assigned(Self.soupravy[index])) then Exit();

 Blky.RemoveSpr(Self.soupravy[index]);
 ORTCPServer.OnRemoveSpr(Self.soupravy[index]);
 FreeAndNil(Self.soupravy[index]);
 SprTableData.reload := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSprDb.UpdateFront();
var i:Integer;
begin
 for i := 0 to _MAX_SPR-1 do
  if (Self.soupravy[i] <> nil) then
    Self.soupravy[i].UpdateFront();
end;

////////////////////////////////////////////////////////////////////////////////

// Tato funkce predpoklada vysokou zatez sbernice do centraly ->
// schvalne ceka.
procedure TSprDb.StopAllSpr();
var i:Integer;
begin
 for i := 0 to _MAX_SPR-1 do
  if ((Self.soupravy[i] <> nil) and (Self.soupravy[i].wantedSpeed <> 0)) then
   begin
    Self.soupravy[i].speed := 0;
    Sleep(3);
   end;
end;

////////////////////////////////////////////////////////////////////////////////

function TSprDb.GetItem(index:Integer):TSouprava;
begin
 Result := Self.soupravy[index];
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSprDb.ClearPOdj();
var i:Integer;
begin
 for i := 0 to _MAX_SPR-1 do
  if (Self.soupravy[i] <> nil) then
    Self.soupravy[i].ClearPOdj();
end;

////////////////////////////////////////////////////////////////////////////////

function TSprDb.GetEmptySpaceForSpr():Integer;
var i:Integer;
begin
 for i := 0 to _MAX_SPR do
   if (Self.soupravy[i] = nil) then
     Exit(i);
 raise Exception.Create('Založen maximální počet souprav!');
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  Soupravy := TSprDb.Create();
finalization
  FreeAndNil(Soupravy);

end.//unit
