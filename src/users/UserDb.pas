unit UserDb;

{
  Trida TUsrDb udrzuje seznam uzivatelu a resi veskere operace s touto
  databazi.
}

interface

uses Generics.Collections, User, IniFiles, Classes, SysUtils, Windows,
     Generics.Defaults, Area, JsonDataObjects;

type
  TUsrDb = class
   private
    users: TObjectList<TUser>;
    ffilenameData: string;
    ffilenameStat: string;

     function GetCount(): Integer;

   public

     constructor Create();
     destructor Destroy(); override;

     procedure LoadAll(const datafn: string; const statefn: string);
     procedure SaveData(const filename: string);
     procedure SaveStat(const filename: string);

     function GetRights(username: string; passwd: string; areaId: string):
                TAreaRights;
     procedure LoginUser(username: string);

     procedure AddUser(User: TUser);
     procedure RemoveUser(index: Integer);

     procedure Sort();
     function IndexOf(username: string): Integer;

     function GetUser(index: Integer): TUser; overload;
     function GetUser(username: string): TUser; overload;

     procedure GetPtData(json: TJsonObject);

     property count: Integer read GetCount;
     property filenameData: string read ffilenameData;
     property filenameStat: string read ffilenameStat;

  end;//class TUserDb

var
  UsrDB: TUsrDb;

implementation

uses Logging, DataUsers, AreaDb, appEv, PTUtils;

////////////////////////////////////////////////////////////////////////////////

constructor TUsrDb.Create();
begin
 inherited Create();
 Self.users := TObjectList<TUser>.Create();
end;//ctor

destructor TUsrDb.Destroy();
begin
 // ulozit statistiku uzivatelu
 if (Self.filenameData <> '') then
  begin
   // kontrola pro pripad zabijeni programu ihned po spusteni
   try
    Self.SaveStat(Self.filenameStat);
   except

   end;
  end;

 Self.users.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TUsrDb.LoadAll(const datafn: string; const statefn: string);
var iniData, iniStat: TMemIniFile;
    str: TStrings;
    i: Integer;
    User: TUser;
begin
 Self.ffilenameData := datafn;
 Self.ffilenameStat := statefn;
 Self.users.Clear();

 writelog('Načítám uživatele...', WR_USERS);

 try
   iniData := TMemIniFile.Create(datafn, TEncoding.UTF8);
   iniStat := TMemIniFile.Create(statefn, TEncoding.UTF8);
 except
   on E: Exception do
    begin
     AppEvents.LogException(E, 'TUsrDb.LoadAll: Ini.Create');
     Exit();
    end;
 end;

 str := TStringList.Create();
 try
   iniData.ReadSections(str);

   for i := 0 to str.Count-1 do
    begin
     try
      User := TUser.Create(iniData, iniStat, str[i]);
      Self.users.Add(User);
     except
      on E : Exception do
        AppEvents.LogException(E, 'Chyba při načítání uživatele '+str[i]);
     end;
    end;//for i
 finally
   str.Free();
   iniData.Free();
   iniStat.Free();
 end;

 Self.users.Sort(TUser.NameComparer());
 writelog('Načteno ' + IntToStr(Self.users.Count) + ' uživatelů', WR_USERS);
end;

procedure TUsrDb.SaveData(const filename: string);
var ini: TMemIniFile;
    user: TUser;
begin
 writelog('Ukládám uživatele...', WR_USERS);

 try
   DeleteFile(PChar(filename));
   ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 except
   on E: Exception do
    begin
     AppEvents.LogException(E, 'TUsrDb.SaveData: DeleteFile, Ini.Create');
     Exit();
    end;
 end;

 try
   for user in Self.users do
     user.SaveData(ini, user.username);

   ini.UpdateFile();
 finally
   ini.Free();
 end;

 writelog('Uživatelé uloženi', WR_USERS);
end;

procedure TUsrDb.SaveStat(const filename: string);
var ini: TMemIniFile;
    user: TUser;
begin
 ini := TMemIniFile.Create(filename+'_', TEncoding.UTF8);
 try
   for user in Self.users do
     user.SaveStat(ini, user.username);
   ini.UpdateFile();

   if (FileExists(filename)) then
     SysUtils.DeleteFile(filename);
   MoveFile(PChar(filename+'_'), PChar(filename));
   SysUtils.DeleteFile(filename+'_');
 finally
   ini.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TUsrDb.GetRights(username: string; passwd: string; areaId: string): TAreaRights;
var user: TUser;
begin
 for user in Self.users do
  if (user.LoginMatch(username, passwd)) then
    Exit(user.GetRights(areaId));
 Result := TAreaRights.null;
end;

////////////////////////////////////////////////////////////////////////////////

function TUsrDb.GetCount(): Integer;
begin
 Result := Self.users.Count;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUsrDb.AddUser(user: TUser);
var muser: TUser;
begin
 for muser in Self.users do
  if (muser.username = user.username) then
    raise Exception.Create('Uživatel s tímto ID již existuje');

 Self.users.Add(user);
 UsersTableData.AddUser();
 Self.Sort();
end;

procedure TUsrDb.RemoveUser(index: Integer);
var areaId: string;
begin
 try
   // nejprve je zapotrebi odpojit vsechny pripojene panely
   for areaId in Self.users[index].areas.Keys do
     if (Areas.Get(areaId) <> nil) then
       Areas.Get(areaId).UserDelete(Self.users[index].username);

   if (Assigned(Self.users[index])) then
     Self.users.Delete(index);
 except

 end;

 UsersTableData.RemoveUser(index);
end;

////////////////////////////////////////////////////////////////////////////////

function TUsrDb.GetUser(index: Integer): TUser;
begin
 if (index >= Self.users.Count) then Exit(nil);
 Result := Self.users.Items[index];
end;

function TUsrDb.GetUser(username: string): TUser;
var index: Integer;
begin
 index := Self.IndexOf(username);
 if (index = -1) then Exit(nil) else Result := Self.users[index];
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUsrDb.LoginUser(username: string);
var i: Integer;
begin
 for i := 0 to Self.users.Count-1 do
  begin
   if (Self.users[i].username = username) then
    begin
     Self.users[i].lastlogin := Now;
     UsersTableData.UpdateLine(i);
     Exit();
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUsrDb.Sort();
begin
 Self.users.Sort(TUser.NameComparer());
 UsersTableData.UpdateTable();
end;

////////////////////////////////////////////////////////////////////////////////

function TUsrDb.IndexOf(username: string): Integer;
var left, right, mid: Integer;
begin
 left  := 0;
 right := Self.users.Count-1;

 while (left <= right) do
  begin
   mid := (left + right) div 2;
   if (Self.users[mid].username = username) then Exit(mid);

   if (CompareStr(username, Self.users[mid].username, loUserLocale) < 0) then
     right := mid - 1
   else
     left := mid + 1;
  end;
 Result := -1;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUsrDb.GetPtData(json: TJsonObject);
var user: TUser;
begin
 json.A['users'];

 for user in Self.users do
  begin
   try
     user.GetPtData(json.A['users'].AddObject);
   except
     on E: Exception do
       PTUtils.PtErrorToJson(json.A['errors'].AddObject,
        '500', 'Chyba pri nacitani uzivatele '+user.username, E.Message);
   end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  UsrDb := TUsrDb.Create();
finalization
  UsrDb.Free();

end.//unit
