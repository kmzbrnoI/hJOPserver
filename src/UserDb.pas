unit UserDb;

interface

uses Generics.Collections, User, IniFiles, Classes, SysUtils, Windows, RPConst;

type
  TUsrDb = class
   private
    Users: TList<TUser>;
    ffilename:string;

      procedure FreeUsers();
      function GetCount():Integer;

   public

     constructor Create();
     destructor Destroy(); override;

     procedure LoadFile(const filename:string);
     procedure SaveFile(const filename:string);

     function GetRights(user:string; passwd:string; OblR:string):TORCOntrolRights;
     procedure LoginUser(username:string);

     procedure AddUser(User:TUser);
     procedure RemoveUser(index:Integer);

     function GetUser(index:Integer):TUser; overload;
     function Getuser(id:string):TUser; overload;
     property count:Integer read GetCount;
     property filename:string read ffilename;

  end;//class TUserDb

var
  UsrDB : TUsrDb;

implementation

uses Logging, DataUsers, TOblsRizeni, TOblRizeni;

////////////////////////////////////////////////////////////////////////////////

constructor TUsrDb.Create();
begin
 inherited Create();
 Self.Users := TList<TUser>.Create();
end;//ctor

destructor TUsrDb.Destroy();
var ini:TMemIniFile;
    i:Integer;
begin
 // ulozit statistiku uzivatelu
 try
   ini := TMemIniFile.Create(filename);

   for i := 0 to Self.Users.Count-1 do
    Self.Users[i].SaveStat(ini, Self.Users[i].id);

   ini.UpdateFile();
   ini.Free();
 except

 end;

 Self.FreeUsers();
 Self.Users.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TUsrDb.FreeUsers();
var i:Integer;
begin
 for i := 0 to Self.Users.Count-1 do
  Self.Users.Items[i].Free();
 Self.Users.Clear();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TUsrDb.LoadFile(const filename:string);
var ini:TMemIniFile;
    str:TStrings;
    i:Integer;
    User:TUser;
begin
 Self.ffilename := filename;
 Self.FreeUsers();

 writelog('Nacitam uzivatele...', WR_USERS);

 try
   ini := TMemIniFile.Create(filename);
 except
   writelog('Pri nacitani uzivatelu nastala chyba - nelze inicializaovat ini objekt', WR_ERROR);
   Exit();
 end;

 str := TStringList.Create();
 ini.ReadSections(str);

 for i := 0 to str.Count-1 do
  begin
   try
    User := TUser.Create(ini, str[i]);
    Self.Users.Add(User);
   except
    on E : Exception do
      writelog('Chyba pri nacitani uzivatele '+str[i]+ ' : '+e.Message, WR_ERROR);
   end;
  end;//for i

 str.Free();
 ini.Free();

 writelog('Nacteno ' + IntToStr(Self.Users.Count) + ' uzivatelu', WR_USERS);
end;//procedure

procedure TUsrDb.SaveFile(const filename:string);
var ini:TMemIniFile;
    i:Integer;
begin
 writelog('Ukladam uzivatele...', WR_USERS);

 try
   DeleteFile(PChar(filename));
   ini := TMemIniFile.Create(filename);
 except
   writelog('Pri ukladani uzivatelu nastala chyba - nelze inicializaovat ini objekt', WR_ERROR);
   Exit();
 end;

 for i := 0 to Self.Users.Count-1 do
  Self.Users[i].SaveData(ini, Self.Users[i].id);

 ini.UpdateFile();
 ini.Free();
 writelog('Uzivatele ulozeni', WR_USERS);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TUsrDb.GetRights(user:string; passwd:string; OblR:string):TORCOntrolRights;
var i:Integer;
begin
 for i := 0 to Self.Users.Count-1 do
  if (Self.Users.Items[i].id = user) then
    if (TUser.ComparePasswd(passwd, Self.Users.Items[i].password)) then   // password check
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
end;//procedure

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

   if (Assigned(Self.Users.Items[index])) then
    begin
     Self.Users.Items[index].Free();
     Self.Users.Delete(index);
    end;
 except

 end;

 UsersTableData.RemoveUser(index);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TUsrDb.GetUser(index:Integer):TUser;
begin
 if (index >= Self.Users.Count) then Exit(nil);
 Result := Self.Users.Items[index];
end;//function

function TUsrDb.Getuser(id:string):TUser;
var i:Integer;
begin
 for i := 0 to Self.Users.Count-1 do
  if (Self.Users[i].id = id) then Exit(Self.Users[i]);
 Exit(nil); 
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
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization
  UsrDb := TUsrDb.Create();
finalization
  UsrDb.Free();

end.//unit
