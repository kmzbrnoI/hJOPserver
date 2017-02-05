unit SprDb;

// databaze souprav

interface

uses SysUtils, Souprava, IniFiles, Classes, Windows, Forms;

const
  _MAX_SPR = 128;

type

  TSprDb = class
   private
      ffilename:string;

      procedure ClearSpr();

      function GetCount():Integer;

   public
      soupravy:array [0.._MAX_SPR] of TSouprava;

      constructor Create();
      destructor Destroy(); override;

      function LoadData(const filename:string):Byte;
      function SaveData(const filename:string):Byte;

      procedure AddSprFromPanel(spr:TStrings; usek:TObject; OblR:TObject);
      procedure RemoveSpr(index:Integer);

      function GetSprNameByIndex(index:Integer):string;
      function GetSprIndexByName(name:string):Integer;

      procedure UpdateFront();
      procedure StopAllSpr();

      property filename:string read ffilename;
      property count:Integer read GetCount;

  end;//TSprDb

var
  Soupravy : TSprDb;

implementation

uses Logging, DataSpr, TBloky, TBlokUsek, DataHV, TBlokSCom, appEv;

////////////////////////////////////////////////////////////////////////////////

constructor TSprDb.Create();
begin
 inherited Create();
 Self.ClearSpr();
end;//ctor

destructor TSprDb.Destroy();
begin
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TSprDb.ClearSpr();
var i:Integer;
begin
 for i := 0 to _MAX_SPR-1 do
  if (Assigned(Self.soupravy[i])) then
   FreeAndNil(Self.soupravy[i]);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TSprDb.LoadData(const filename:string):Byte;
var ini:TMemIniFile;
    i:Integer;
    sections:TStrings;
begin
 writelog('Nacitam soupravy: '+filename, WR_DATA);
 Self.ffilename := filename;

 try
   ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 except
   on E:Exception do
    begin
     AppEvents.LogException(E, 'Nacitam soupravy: nelze otevrit soubor souprav');
     Exit(1);
    end;
 end;

 sections := TStringList.Create();
 ini.ReadSections(sections);

 Self.ClearSpr();

 for i := 0 to sections.Count-1 do
   Self.soupravy[i] := TSouprava.Create(ini, sections[i], i);

 writelog('Nacteno '+IntToStr(sections.Count)+' souprav', WR_DATA);

 FreeAndNil(ini);
 FreeAndNil(sections);

 SprTableData.LoadToTable;

 HVTableData.LoadToTable();
 Result := 0;
end;//function

function TSprDb.SaveData(const filename:string):Byte;
var ini:TMemIniFile;
    i:Integer;
begin
 writelog('Ukladam soupravy: '+filename, WR_DATA);

 try
   DeleteFile(PChar(filename));
   ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 except
   on E:Exception do
    begin
     AppEvents.LogException(E, 'Ukladam soupravy: nelze otevrit soubor souprav');
     Exit(1);
    end;
 end;

 for i := 0 to _MAX_SPR-1 do
   if (Assigned(Self.soupravy[i])) then
     Self.soupravy[i].SaveToFile(ini, IntToStr(i));

 ini.UpdateFile();
 FreeAndNil(ini);

 writelog('Ulozeno '+IntToStr(Self.Count)+' souprav', WR_DATA);
 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TSprDb.GetCount():Integer;
var i:Integer;
begin
 Result := 0;
 for i := 0 to _MAX_SPR do
  if (Assigned(Self.soupravy[i])) then
   Inc(Result);
end;//function

////////////////////////////////////////////////////////////////////////////////

function TSprDb.GetSprNameByIndex(index:Integer):string;
begin
 if (index < 0) or (index >= _MAX_SPR) then Exit('-');

 if (Assigned(Self.soupravy[index])) then
  Result := Self.soupravy[index].nazev
 else
  Result := '-';
end;//function

////////////////////////////////////////////////////////////////////////////////

function TSprDb.GetSprIndexByName(name:string):Integer;
var i:Integer;
begin
 for i := 0 to _MAX_SPR-1 do
  if ((Assigned(Self.soupravy[i])) and (Self.soupravy[i].nazev = name)) then
   Exit(i);
 Exit(-1);
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TSprDb.AddSprFromPanel(spr:TStrings; Usek:TObject; OblR:TObject);
var i:Integer;
begin
 for i := 0 to _MAX_SPR-1 do
  if (Self.soupravy[i] = nil) then
   begin
    try
      Self.soupravy[i] := TSouprava.Create(spr, Usek, i, OblR);
      if (Assigned(Usek)) then          // toto musi byt tady, nikoliv v konstruktoru
       begin
        (Usek as TBlkUsek).Souprava := i;
        (Usek as TBlkUsek).Change();    // volano kvuli aktualizaci dat
        if ((Usek as TBlkUsek).SComJCRef <> nil) then
          ((Usek as TBlkUsek).SComJCRef as TBlkScom).UpdateRychlostSpr(true);
       end;
      Exit();
    except
     on E: Exception do
      begin
       FreeAndNil(Self.soupravy[i]);
       SprTableData.reload := true;
       raise Exception.Create(E.Message);
       Exit();
      end;
    end;
   end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TSprDb.RemoveSpr(index:Integer);
begin
 if (not Assigned(Self.soupravy[index])) then Exit();

 Blky.RemoveSpr(index);
 FreeAndNil(Self.soupravy[index]);
 SprTableData.reload := true;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TSprDb.UpdateFront();
var i:Integer;
begin
 for i := 0 to _MAX_SPR-1 do
  if (Self.soupravy[i] <> nil) then
    Self.soupravy[i].UpdateFront();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// Tato funkce predpoklada vysokou zatez sbernice do centraly ->
// schvalne ceka.
procedure TSprDb.StopAllSpr();
var i:Integer;
begin
 for i := 0 to _MAX_SPR-1 do
  if ((Self.soupravy[i] <> nil) and (Self.soupravy[i].rychlost <> 0)) then
   begin
    Self.soupravy[i].rychlost := 0;
    Sleep(3);
   end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization
  Soupravy := TSprDb.Create();
finalization
  FreeAndNil(Soupravy);

end.//unit
