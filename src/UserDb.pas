unit UserDb;

{
  Trida TUsrDb udrzuje seznam uzivatelu a resi veskere operace s touto
  databazi.
}

interface

uses Generics.Collections, User, IniFiles, Classes, SysUtils, Windows,
     Generics.Defaults, TOblRizeni;

type
  TUsrDb = class
   private
    Users: TObjectList<TUser>;                                                  // seznam uzivatelu
    ffilenameData:string;                                                       // jmeno ini souboru s daty uzivatelu, ze ktereho jsme naposled nacitali data
    ffilenameStat:string;

      function GetCount():Integer;                                              // vrati pocet uzivatelu

   public

     constructor Create();
     destructor Destroy(); override;

     procedure LoadAll(const datafn:string; const statefn:string);
     procedure SaveData(const filename:string);
     procedure SaveStat(const filename:string);

     function GetRights(user:string; passwd:string; OblR:string):               // vrati prava uzivatele \user s heslem \passwd pro oblast rizeni \OblR
                TORCOntrolRights;
     procedure LoginUser(username:string);                                      // zaloguje prihlaseni uzivatele (ulozi datum a cas posledniho prihlaseni)

     procedure AddUser(User:TUser);
     procedure RemoveUser(index:Integer);

     procedure Sort();
     function IndexOf(id:string):Integer;

     function GetUser(index:Integer):TUser; overload;                           // vrati uzivatele podle jeho indexu v senzamu univatelu
     function GetUser(id:string):TUser; overload;
     property count:Integer read GetCount;                                      // vrati pocet uzivatelu
     property filenameData:string read ffilenameData;                                   // vrati jsmeno souboru, ze ktereho byly uzivatele necteni
     property filenameStat:string read ffilenameStat;

  end;//class TUserDb

var
  UsrDB : TUsrDb;

implementation

uses Logging, DataUsers, TOblsRizeni, appEv;

////////////////////////////////////////////////////////////////////////////////

constructor TUsrDb.Create();
begin
 inherited Create();
 Self.Users := TObjectList<TUser>.Create();
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

 Self.Users.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TUsrDb.LoadAll(const datafn:string; const statefn:string);
var iniData, iniStat:TMemIniFile;
    str:TStrings;
    i:Integer;
    User:TUser;
begin
 Self.ffilenameData := datafn;
 Self.ffilenameStat := statefn;
 Self.Users.Clear();

 writelog('Naèítám uživatele...', WR_USERS);

 try
   iniData := TMemIniFile.Create(datafn, TEncoding.UTF8);
   iniStat := TMemIniFile.Create(statefn, TEncoding.UTF8);
 except
   on E:Exception do
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
      Self.Users.Add(User);
     except
      on E : Exception do
        AppEvents.LogException(E, 'Chyba pøi naèítání uživatele '+str[i]);
     end;
    end;//for i
 finally
   str.Free();
   iniData.Free();
   iniStat.Free();
 end;

 Self.Users.Sort(TComparer<TUser>.Construct(TUser.comparer));
 writelog('Naèteno ' + IntToStr(Self.Users.Count) + ' uživatelù', WR_USERS);
end;

procedure TUsrDb.SaveData(const filename:string);
var ini:TMemIniFile;
    user:TUser;
begin
 writelog('Ukládám uživatele...', WR_USERS);

 try
   DeleteFile(PChar(filename));
   ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 except
   on E:Exception do
    begin
     AppEvents.LogException(E, 'TUsrDb.SaveData: DeleteFile, Ini.Create');
     Exit();
    end;
 end;

 try
   for user in Self.Users do
     user.SaveData(ini, user.login);

   ini.UpdateFile();
 finally
   ini.Free();
 end;

 writelog('Uživatelé uloženi', WR_USERS);
end;

procedure TUsrDb.SaveStat(const filename: string);
var ini:TMemIniFile;
    user:TUser;
begin
 ini := TMemIniFile.Create(filename+'_', TEncoding.UTF8);
 try
   for user in Self.Users do
     user.SaveStat(ini, user.login);
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

function TUsrDb.GetRights(user:string; passwd:string; OblR:string):TORCOntrolRights;
var i:Integer;
begin
 for i := 0 to Self.Users.Count-1 do
  if (Self.Users.Items[i].id = user) then
    if (TUser.ComparePasswd(passwd, Self.Users.Items[i].password, Self.Users.Items[i].salt)) then   // password check
      Exit(Self.Users.Items[i].GetRights(OblR));
 Result := TORControlRights.null;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TUsrDb.GetCount():Integer;
begin
 Result := Self.Users.Count;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TUsrDb.AddUser(User:TUser);
var i:Integer;
begin
 for i := 0 to Self.Users.Count-1 do
  if (Self.Users.Items[i].id = User.id) then
    raise Exception.Create('Uživatel s tímto ID již existuje');

 Self.Users.Add(User);
 UsersTableData.AddUser();
 Self.Sort();
end;

procedure TUsrDb.RemoveUser(index:Integer);
var oblr:string;
    OblRRef:TOR;
begin
 try
   // nejprve je zapotrebi odpojit vsechny pripojene panely
   for oblr in Self.Users[index].OblR.Keys do
    begin
     ORs.GetORByIndex(ORs.GetORIndex(oblr), OblRRef);
     if (OblRRef <> nil) then OblRRef.UserDelete(Self.Users[index].id);
    end;

   if (Assigned(Self.Users[index])) then
     Self.Users.Delete(index);
 except

 end;

 UsersTableData.RemoveUser(index);
end;

////////////////////////////////////////////////////////////////////////////////

function TUsrDb.GetUser(index:Integer):TUser;
begin
 if (index >= Self.Users.Count) then Exit(nil);
 Result := Self.Users.Items[index];
end;//function

function TUsrDb.GetUser(id:string):TUser;
var index:Integer;
begin
 index := Self.IndexOf(id);
 if (index = -1) then Exit(nil) else Result := Self.Users[index];
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TUsrDb.LoginUser(username:string);
var i:Integer;
begin
 for i := 0 to Self.Users.Count-1 do
  if (Self.Users[i].id = username) then
   begin
    Self.Users[i].lastlogin := Now;
    UsersTableData.UpdateLine(i);
    Exit();
   end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUsrDb.Sort();
begin
 Self.Users.Sort(TComparer<TUser>.Construct(TUser.comparer));
 UsersTableData.UpdateTable();
end;

////////////////////////////////////////////////////////////////////////////////

function TUsrDb.IndexOf(id:string):Integer;
var left, right, mid: Integer;
begin
 left  := 0;
 right := Self.Users.Count-1;

 while (left <= right) do
  begin
   mid := (left + right) div 2;
   if (Self.Users[mid].id = id) then Exit(mid);

   if (AnsiCompareStr(id, Self.Users[mid].id) < 0) then
     right := mid - 1
   else
     left := mid + 1;
  end;
 Result := -1;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  UsrDb := TUsrDb.Create();
finalization
  UsrDb.Free();

end.//unit
